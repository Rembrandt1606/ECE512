class PFModuleMemImp(outer: PFExampleMem)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
  with HasCoreParameters
 {
//val cacheParams = tileParams.dcache.get
//val busy = RegInit(VecInit(Seq.fill(outer.n){false.B}))
val busy = Reg(Bool())
val regfile = Mem(outer.n, UInt(64.W))
val regSHR = RegInit(VecInit(Seq.fill(outer.n)(0.U(2.W)))) // Status Handeling Register File
 // TWO bits:
// bit 1 is valid bit (has this data been used), if 1 data needs to be replaced
// bit 2 is src array tag, and is equal to log2Ciel(num_sources_vectors). This is needed due to out of order responses from memory interface
val file_full = Reg(Bool())
val has_valid_data = Reg(Bool())
// Floating Point Addition Module
val fpAdder = Module(new AddRecFN(outer.expWidth, outer.sigWidth))
val fpAddSHR = Reg(UInt(5.W)) // Add Status Handling Register
// Floating Point Multiply Accumulate Module
//val fpMAC = Module(new MulAddRecFN(outer.expWidth, outer.sigWidth))
//val fpMACSHR = Reg(UInt(5.W)) // MAC Status Handling Register
 
val fpDiv = Module(new DivSqrtRecFN_small(outer.expWidth, outer.sigWidth, 0))
val fpDivSHR = Reg(UInt(5.W))
// CumSum Instruciton
// ------------------------------------
// funct7  --> 0
// rs2     --> contains lenght of array to cumlative sum over
// rs1     --> contains ptr to start address
// xd      --> set to 1, core waits for response
// xs1/xs2 --> set to 1, are using both rs1 and rs2
// rd      --> location to send cumlative sum
// opcode  --> 01
// ------------------------------------
//
// Read Divide Write Instruction
// funct7  --> 1
// rs2     --> Not Used
// rs1     --> contains ptr to weights
// xd      --> set to 0, core does not wait for response (need to ASM after)
// xs1/xs2 --> set to 0, are not using both rs1 and rs2
// rd      --> location to send done signal
// opcode  --> 01
// ========================= CORE INPUT INTERFACE ========================= //
val core_cmd = Queue(io.cmd)                       // FIFO cmd from core
val core_response = io.resp
val mem_request = io.mem.req
val rocc_funct = core_cmd.bits.inst.funct  // function from incoming RoCC instruction
val rocc_rs2   = core_cmd.bits.rs2         // src1 register data
val rocc_xd    = core_cmd.bits.inst.xd     // true if core needs reponse
val rocc_rd    = core_cmd.bits.inst.rd     // destination register
val rocc_rs1   = core_cmd.bits.rs1         // src2 register data
val rocc_op    = core_cmd.bits.inst.opcode // 0x1 (acclerator ID = 1)
val rocc_dprv  = core_cmd.bits.status.dprv // CSR content, needed for appropriate prilvages in cache
val rocc_dv    = core_cmd.bits.status.dv   // NOT SURE
val doCumSum = rocc_funct === 0.U          // When function is 0 we want to cumlative sum
val doDiv  = rocc_funct === 1.U            // When function is 1 we want to multiply accumlate
// Internal Registers
val sum_weights = Reg(UInt(64.W))                  // To store double of sum weights
val weights_start_addr = Reg(UInt(coreMaxAddrBits.W))   // start address in memory, contained in rs1 of op 0 (Weights Start Address in memory)
 
val curr_addr = Reg(UInt(coreMaxAddrBits.W))       // current address we are pulling from L1
val curr_wb_addr = Reg(UInt(coreMaxAddrBits.W))    // current address we are writing back to
val counter =  Reg(UInt(32.W))                     // counter for the number of addresses that have been accessed
val write_counter = Reg(UInt(32.W))
val array_size = Reg(UInt(32.W))                   // Stores current size of particle filter (n = 10000) in example
 
val resp_rd = Reg(chiselTypeOf(rocc_rd))           // return register ID
 
val a2_start_addr = Reg(UInt(coreMaxAddrBits.W))
val a3_start_addr = Reg(UInt(coreMaxAddrBits.W))
 
val current_dprv = Reg(chiselTypeOf(rocc_dprv))
val current_dv   = Reg(chiselTypeOf(rocc_dv))
val finished     = Reg(Bool())
val memRespTag = io.mem.resp.bits.tag(1,0)
val memAddrOffset = RegInit(outer.precision.U(4.W)) //4 for 32 8.U for 64)
val addr_tag = Reg(UInt(2.W))
val addr_access_tag = Reg(UInt(2.W))
val rocc_busy = Reg(Bool())
// s_idle    --> awaiting an incoming op (can we clock gate)
// s_acquire --> request to access memory location
// s_grant   --> request granted at memory location
// s_sum     --> adding incoming value to cumSum
// s_retrun  --> return computed cumlative sum
//val s_idle :: s_wait :: s_grant :: s_acquire_more :: s_sum :: s_return :: Nil = Enum(7)
val m_idle :: m_wait :: m_stream_read1 :: m_writeback :: m_w_wait :: Nil = Enum(5)
val r_idle :: r_busy :: r_return :: Nil = Enum(3)
val c_idle :: c_add :: c_div :: c_wait_div :: Nil = Enum(4)
 
val comp_state = RegInit(c_idle)
val mem_state = RegInit(m_idle)
val rocc_state = RegInit(r_idle)
val mem_finished = Reg(Bool())
val mem_w_finished = Reg(Bool())
val add_finished = Reg(Bool())
val div_finished = Reg(Bool())
val need_writeback = Reg(Bool())
val writeback_data = Reg(UInt(64.W))
val twoSRC = Reg(Bool())
val writing = Reg(Bool())
val sumWeights =  Reg(UInt(64.W))
val curOp = Reg(UInt(2.W))
// CORE INTERFACE CONTROL SIGNALS
 
// Define i_fifo_1 queues and IO
val rocc_interal = Wire(new Bundle {
   val in_data_1 = Flipped(Decoupled(UInt(64.W)))
   val out_data_1 = Decoupled(UInt(64.W))
 
   val in_data_w = Flipped(Decoupled(UInt(64.W)))
   val out_data_w = Decoupled(UInt(64.W))
})
 
val i_fifo_1 = Queue(rocc_interal.in_data_1, 3)
 
val w_fifo = Queue(rocc_interal.in_data_w, 3)
 
rocc_interal.out_data_1 <> i_fifo_1
rocc_interal.out_data_w <> w_fifo
 
core_cmd.ready := (!rocc_busy)           // Only ready in idle state
core_response.valid := (rocc_state === r_return)    // Valid return when in return state
core_response.bits.rd := resp_rd               // always return the response to the correct address
core_response.bits.data := sum_weights //fNFromRecFN(outer.expWidth, outer.sigWidth, testreg1)              // sematanics always return Cumlative Sum
io.busy := rocc_busy
 io.mem.req.valid := false.B   // ((state === s_acquire || state === s_acquire_more)
  // && !busy && !(curr_addr === end_addr || counter >= 10.U))  // Valid request when we are attempting to aquire
writing := false.B
mem_finished := (counter === array_size && rocc_busy)
mem_w_finished := (write_counter === array_size && rocc_busy)
add_finished := (mem_finished && !i_fifo_1.valid && comp_state =/= c_add && curOp === 0.U)
div_finished := (mem_finished && !i_fifo_1.valid && mem_w_finished && curOp === 1.U)
 
fpAdder.io.subOp := false.B
fpAdder.io.a := 0.U // Mux(state === s_sum, testreg1, 0.U)
fpAdder.io.b := 0.U // Mux(state === s_sum, recFNFromFN(outer.expWidth, outer.sigWidth, sum_weights), 0.U)
fpAdder.io.roundingMode := 0.U // TO NEAREST FP
fpAdder.io.detectTininess := false.B
fpAddSHR := fpAdder.io.exceptionFlags
 
 
fpDiv.io.sqrtOp := false.B
fpDiv.io.roundingMode := 0.U
fpDivSHR := fpDiv.io.exceptionFlags//Cat(fpDiv.io.roundingModeOut, Cat(fpDiv.io.invalidExc.asUInt, fpDiv.io.infiniteExc.asUInt)) // Output status after FP Div
fpDiv.io.detectTininess := false.B
fpDiv.io.inValid := false.B
 // ============================================
 // INITALIZE SO CHISLE DOESNT COMPLAIN
rocc_interal.in_data_1.bits := 0.U        
rocc_interal.out_data_1.ready := false.B
rocc_interal.in_data_1.valid := false.B
rocc_interal.in_data_w.bits := fNFromRecFN(outer.expWidth, outer.sigWidth, fpDiv.io.out)
rocc_interal.in_data_w.valid := fpDiv.io.outValid_div
need_writeback := rocc_interal.out_data_w.valid
rocc_interal.out_data_w.ready := false.B

fpDiv.io.a := 0.U
fpDiv.io.b := 0.U
// ===============================================================================
// When we have a valid core command inc and it is compute cummulative sum
switch(rocc_state) {
    is(r_idle)
    {
        when(core_cmd.fire() && !rocc_busy){
            when(doCumSum){
               weights_start_addr := rocc_rs1
               array_size := rocc_rs2
               curr_addr := rocc_rs1
               // Initalize interal registers
               counter := 0.U
               sumWeights := 0.U
               sum_weights := 0.U
               addr_tag := 0.U
               curOp := 0.U
               current_dprv := rocc_dprv
               current_dv   := rocc_dv
               resp_rd := rocc_rd
               twoSRC := false.B
               //need_writeback := false.B
               rocc_state := r_busy
               rocc_busy := true.B
 
            }.elsewhen(doDiv) {
 
               curr_addr := rocc_rs1
               current_dprv := rocc_dprv
               current_dv   := rocc_dv
               curr_wb_addr := rocc_rs1
               curOp := 1.U
               counter := 0.U
               write_counter := 0.U
               resp_rd := 0.U
               rocc_state := r_busy
               rocc_busy := true.B
               twoSRC := false.B
               // need_writeback := true.B
 
            }.otherwise{
              rocc_busy := false.B
            }
        }
    }
   
    is(r_busy){
      when(add_finished && curOp === 0.U){
        rocc_state := r_return
      }.elsewhen(div_finished && curOp === 1.U)
      {
        rocc_state := r_idle
        rocc_busy := false.B
      }
      .otherwise{
        rocc_state := r_busy
      }
    }
}
// Memory Interface Read Controller
switch(mem_state){
   is(m_idle){
       // When we want to do work and i_fifo_1 queue is not full
       //memData := 0.U
       val read_ready = rocc_busy
       when(read_ready){
         // Send out read request
           mem_state := m_stream_read1
       }.otherwise{
           mem_state := m_idle
       }
   }
   is(m_stream_read1)
   {
      when(need_writeback){
          mem_state := m_writeback
      }
      .elsewhen((counter < array_size))
       {
           when(twoSRC) // Special case for two source arrays needed for multiply accumulate
           {
               // TODO
           }
           .otherwise // If one source array
           {
               // send our memory request
               io.mem.req.valid := rocc_interal.in_data_1.ready
               io.mem.req.bits.dprv := current_dprv
               io.mem.req.bits.dv := current_dv
               io.mem.req.bits.addr := curr_addr
               io.mem.req.bits.tag := addr_tag
               io.mem.req.bits.cmd := M_XRD // load
               io.mem.req.bits.size := log2Ceil(outer.precision).U  // 64 bits
               when(io.mem.req.valid && io.mem.req.ready) { // When we send off the request
                   addr_tag := addr_tag + 1.U // iterate the address we want to access
                   counter := counter + 1.U // iterate the counter
                   mem_state := m_wait // wait for response
                   curr_addr := curr_addr + memAddrOffset
               }.otherwise{
                   mem_state := m_stream_read1 // stay in read 1 state
               }
           }
       }.otherwise
       {
           mem_state := m_idle
       }
   }
 
   is(m_writeback)
    {
        rocc_interal.out_data_w.ready := true.B
      // when(!writing && counter < array_size) // When we are not writing and counter is less than the size of the array
      when((write_counter < array_size))
       {              
           when(twoSRC) // Special case for two source arrays needed for multiply accumulate
           {
               // TODO
           }
           .otherwise // If one source array
           {
               // send our memory request
               io.mem.req.valid := rocc_interal.out_data_w.ready
               io.mem.req.bits.dprv := current_dprv
               io.mem.req.bits.dv := current_dv
               io.mem.req.bits.addr := curr_wb_addr
               io.mem.req.bits.tag := addr_tag
               io.mem.req.bits.data := rocc_interal.out_data_w.bits
               io.mem.req.bits.cmd := M_XWR // store
               io.mem.req.bits.size := log2Ceil(outer.precision).U  // 64 bits
               when(io.mem.req.valid && io.mem.req.ready)
               { // When we send off the request
                   addr_tag := addr_tag + 1.U // iterate the address we want to access
                   write_counter := write_counter + 1.U // iterate the counter
                   mem_state := m_w_wait // wait for response
                   curr_wb_addr := curr_wb_addr + memAddrOffset
               }.otherwise
               {
                   mem_state := m_writeback // stay in read 1 state
               }
           }
       }.otherwise
       {
           mem_state := m_idle
       }
   }
 
   is(m_wait){
     io.mem.req.valid := false.B
     when(io.mem.resp.valid){ // If reponse has come in assign incoming data to reg file and goto sum
       rocc_interal.in_data_1.valid := true.B
       rocc_interal.in_data_1.bits := io.mem.resp.bits.data
       mem_state := m_stream_read1
     }.otherwise {
       mem_state := m_wait
     }
   }
   
   is(m_w_wait){
       io.mem.req.valid := false.B
       when(io.mem.resp.valid)
       {
           when(need_writeback){
               mem_state := m_writeback
           }.otherwise{
               mem_state := m_stream_read1
           }
       }
   }
}
 
 
switch(comp_state){
   is(c_idle){
       val comp_add = (rocc_busy && !add_finished && rocc_interal.out_data_1.valid && curOp === 0.U)
       val comp_div = (rocc_busy && !div_finished && rocc_interal.out_data_1.valid && curOp === 1.U)
       when(comp_add)
        {
          comp_state := c_add
        }.elsewhen(comp_div){
          comp_state := c_div
        }
         .otherwise{
           comp_state := c_idle
       }
   }
   is(c_add){
    rocc_interal.out_data_1.ready := true.B
    when(rocc_interal.out_data_1.fire())
       {
            fpAdder.io.a := recFNFromFN(outer.expWidth, outer.sigWidth, rocc_interal.out_data_1.bits)
            fpAdder.io.b := recFNFromFN(outer.expWidth, outer.sigWidth, sum_weights)
            sum_weights := fNFromRecFN(outer.expWidth, outer.sigWidth,fpAdder.io.out)
       }
       .otherwise{
           comp_state := c_idle
       }
   }
   is(c_div){
    fpDiv.io.inValid := rocc_interal.out_data_1.valid
    rocc_interal.out_data_1.ready := fpDiv.io.inReady
    when(rocc_interal.out_data_1.fire())  
        {
            fpDiv.io.a := recFNFromFN(outer.expWidth, outer.sigWidth, rocc_interal.out_data_1.bits)
            fpDiv.io.b := recFNFromFN(outer.expWidth, outer.sigWidth, sumWeights)
            comp_state := c_wait_div
        }
   }
   is(c_wait_div)
   {
     when(fpDiv.io.outValid_div){
        when(rocc_interal.out_data_1.valid){
          comp_state := c_div
        }.otherwise{
          comp_state := c_idle
        }
     }.otherwise{
       comp_state := c_wait_div
     }
   }
}
 
when (core_response.fire()) {
    rocc_state := r_idle
    sumWeights := sum_weights
    rocc_busy := false.B
}
//
io.interrupt := false.B
// =========================  MEMORY REQUEST UNUSED =========================== //
io.mem.req.bits.signed := false.B
//io.mem.req.bits.data := 0.U // we're not performing any stores...
io.mem.req.bits.phys := false.B
// ===================================================================== //
}
