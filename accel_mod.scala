class WithRoccAccleratorMem extends Config((site, here, up) => {
 case BuildRoCC => List(
   (p: Parameters) => {
       val memtestr = LazyModule(new PFExampleMem(OpcodeSet.custom0, n = 4)(p))
       memtestr
   })
})

class PFExampleMem(opcodes: OpcodeSet, val n: Int = 4, val expWidth: Int = 11, val sigWidth: Int = 53,
val precision: Int = 8, val expDegree: Int = 8, val numDivUnits: Int = 16)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new PFModuleMemImp(this) // n = number of outstanding memory operations
}

class PFModuleMemImp(outer: PFExampleMem)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
with HasCoreParameters
{
// Define internal routing
val rocc_interal = Wire(new Bundle {
     val in_data_1  = Flipped(Decoupled(UInt(64.W)))
     val in_data_2  = Flipped(Decoupled(UInt(64.W)))
     val in_addr_r = Flipped(Decoupled(UInt(coreMaxAddrBits.W)))
     val in_data_w  = Flipped(Decoupled(UInt(64.W)))
     val in_addr_w = Flipped(Decoupled(UInt(coreMaxAddrBits.W)))
 
     val out_data_1 = Decoupled(UInt(64.W))
     val out_data_2 = Decoupled(UInt(64.W))
     val out_data_w = Decoupled(UInt(64.W))
     val out_addr_r = Decoupled(UInt(coreMaxAddrBits.W))
     val out_addr_w = Decoupled(UInt(coreMaxAddrBits.W))
 })
// FIFOS TO DECOUPLE MEMORY READ/WRITE FROM CALCUATION
// can add additonal FP Units to increase throughput
// would need to add an interal tag tracking mechanism
// Input fifos to floating point units
val i_fifo_1 = Queue(rocc_interal.in_data_1, 5)
val i_fifo_2 = Queue(rocc_interal.in_data_2, 5)
// writeback fifo
val w_fifo = Queue(rocc_interal.in_data_w, 5)
val w_addr_fifo = Queue(rocc_interal.in_addr_w, 5)
val r_addr_fifo = Queue(rocc_interal.in_addr_r, 5)
 
val expValues = new ArrayBuffer[UInt]
// calcuate 1/N! for 0 to expApprox N's
// N! = 1 * 2 * 3 * ... * N
// Just going to hardcoded LUT, could not get double to be cast to UInt otherwise
expValues += (0x3FF0000000000000L).asUInt(64.W) // 1/0! = 1
expValues += (0x3FF0000000000000L).asUInt(64.W) // 1/1! = 1
expValues += (0x3FE0000000000000L).asUInt(64.W) // 1/2! = 0.5
expValues += (0x3FC555555567A895L).asUInt(64.W) // 1/3! = 0.16666
expValues += (0x3FA55555555CA9D5L).asUInt(64.W) // 1/4! = 0.04166
expValues += (0x3F811111110E2277L).asUInt(64.W) // 1/5! = 0.00833
// Creating a look up table to simplify factorial division for e^x taylor series calcuation
// Only 4th order is needed for 99.99% accruacy (3rd order is 98% for 1 less cycle)
val expLUT = VecInit(expValues)
// Writeback register file size = 2 for test
val register_file = RegInit(VecInit(Seq.fill(outer.numDivUnits)(0.U(coreMaxAddrBits.W)))) // Writeback Addr file
val w_reg_file = RegInit(VecInit(Seq.fill(outer.numDivUnits)(0.U(64.W))))   // Writeback data
val div_occupied = RegInit(VecInit(Seq.fill(outer.numDivUnits)(0.U(1.W))))  // Is div unit currently calculating
val div_w_ready = RegInit(VecInit(Seq.fill(outer.numDivUnits)(0.U(1.W))))   // Is writeback data valid
 
// Connect queues
rocc_interal.out_data_1 <> i_fifo_1
rocc_interal.out_data_2 <> i_fifo_2
rocc_interal.out_data_w <> w_fifo
rocc_interal.out_addr_w <> w_addr_fifo
rocc_interal.out_addr_r <> r_addr_fifo
// === DEFINE FP MODULES === //
// Floating Point Addition Unit (1 cycle)
val fpAdder   = Module(new AddRecFN(outer.expWidth, outer.sigWidth))
val fpAddSHR  = Reg(UInt(5.W)) // Add Exception Handling Register
// Floating POint Division/Squart Unit (~20 - 64 cycles)
//val fpDiv     = Module(new DivSqrtRecFN_small(outer.expWidth, outer.sigWidth, 0))
//val fpDivSHR  = Reg(UInt(5.W)) // Div Exception Handling Register
 
val div_units = for (i <- 0 until outer.numDivUnits) yield
{
   val div_unit = Module(new DivSqrtRecFN_small(outer.expWidth, outer.sigWidth, 0))
   div_unit
}
 
val div_units_io = VecInit(div_units.map(_.io))
val fpDivSHRs = RegInit(VecInit(Seq.fill(outer.numDivUnits)(0.U(5.W))))
 
// Floating Point Multiply Accumulate Unit (1 cycle)
val fpMAC     = Module(new MulAddRecFN(outer.expWidth, outer.sigWidth))
val fpMACSHR  = Reg(UInt(5.W)) // MAC Exception Handling Register
// Floating Point Multiply Unit (1 cycle) NOTE: e^y calculation will be multicycle
val fpMul     = Module(new MulRecFN(outer.expWidth, outer.sigWidth))
val fpMulSHR  = Reg(UInt(5.W)) // Mul Exception Handling Register
// Floating Point Multiply Unit (1 cycle) NOTE: e^y calculation will be multicycle
val fpMul2    = Module(new MulRecFN(outer.expWidth, outer.sigWidth))
val fpMul2SHR = Reg(UInt(5.W)) // Mul Exception Handling Register
// CURRENT ITERATION OF CUSTOM INSTRUCTIONS
// Run Sum (Initalization) Instruciton
// ------------------------------------
// funct   --> 0
// rs2     --> contains lenght of array to cumlative sum over
// rs1     --> contains ptr to start address
// xd      --> set to 1, core waits for response
// xs1/xs2 --> set to 1, are using both rs1 and rs2
// rd      --> location to send cumlative sum
// opcode  --> 00
//
// Normalize Weights Instruction
// ------------------------------------
// funct  --> 1
// rs2     --> Not Used
// rs1     --> contains ptr to weights
// xd      --> set to 0, core does not wait for response (need to ASM after)
// xs1/xs2 --> set to 0, are not using both rs1 and rs2
// rd      --> location to send done signal
// opcode  --> 00
//
// Multiply Accumulate Instruction
// ------------------------------------
// Multiply Accumulate c += (b x a) NOTE: a is weights[x], address should be presesnt in co-processor already
// funct   --> 2
// rs2     --> contains ptr to b in memory
// rs1     --> contains ptr to a in memory
// xd      --> set to 1, core waits for response
// xs1/xs2 --> set to 1, are using both rs1 and rs2
// rd      --> location to send c
// opcode  --> 00
//
// Exponential weight & likelihood Instruction
// ------------------------------------
// x = x * e^y Instruction
// funct   --> 3
// rs2     --> contains ptr to x in memory
// rs1     --> contains ptr to y in memory
// xd      --> set to 0, core does not wait for response (need to ASM after)
// xs1/xs2 --> set to 1, are using both rs1 and rs2
// rd      --> location to send c
// opcode  --> 00
// ========================= CORE INPUT INTERFACE ========================= //
val core_cmd      = Queue(io.cmd)                       // FIFO cmd from core
val core_response = io.resp
val mem_request   = io.mem.req
val rocc_funct    = core_cmd.bits.inst.funct  // function from incoming RoCC instruction
val rocc_rs2      = core_cmd.bits.rs2         // src1 register data
val rocc_xd       = core_cmd.bits.inst.xd     // true if core needs reponse
val rocc_rd       = core_cmd.bits.inst.rd     // destination register
val rocc_rs1      = core_cmd.bits.rs1         // src2 register data
val rocc_op       = core_cmd.bits.inst.opcode // 0x1 (acclerator ID = 1)
val rocc_dprv     = core_cmd.bits.status.dprv // CSR content, needed for appropriate prilvages in cache
val rocc_dv       = core_cmd.bits.status.dv   // NOT SURE
val doInit        = rocc_funct === 0.U        // When function is 0 we want to initalize the co-processor
val doDiv         = rocc_funct === 1.U        // When function is 1 we want to writeback divide
val doMAC         = rocc_funct === 2.U        // When function is 2 we want to multiply accumlate
val doExp         = rocc_funct === 3.U        // When function is 3 we want to compute modify weights
// ======================================================================== //
// =========== Pipeline states =========== //
// Memory interface limitation, can not read/write at same time
// limited to 64 bits total access at a time, meaning we can access 2 floats but only 1 double
val m_idle :: m_wait :: m_stream_read1 :: m_writeback :: m_w_wait :: Nil = Enum(5)
val r_idle :: r_busy :: r_return :: Nil = Enum(3)
val c_idle :: c_add :: c_div :: c_mac :: c_exp :: c_exp2 :: c_exp3 :: c_exp_w :: Nil = Enum(8)
val comp_state  = RegInit(c_idle)
val mem_state   = RegInit(m_idle)
val rocc_state  = RegInit(r_idle)
// ================================================================ //
// Internal Registers
val rocc_busy = Reg(Bool())                             // is the co-processor busy
val sum_weights        = Reg(UInt(64.W))                // to store double of sum weights
val mac_total          = Reg(UInt(64.W))
val expo_total         = Reg(UInt(64.W))
val weights_start_addr = Reg(UInt(coreMaxAddrBits.W))   // start address in memory, contained in rs1 of op 0 (Weights Start Address in memory)
val curr_a_addr     = Reg(UInt(coreMaxAddrBits.W))      // current fifo 1 address we are pulling from L1
val curr_b_addr     = Reg(UInt(coreMaxAddrBits.W))      // current fifo 2 address we are pulling from L1
val curr_c_addr     = Reg(UInt(coreMaxAddrBits.W))      // current fifo 3 address we are pulling from L1
val curr_wb_addr  = Reg(UInt(coreMaxAddrBits.W))        // current address we are writing back to
val counter       =  Reg(UInt(32.W))                    // counter for the number of addresses that have been accessed
val write_counter = Reg(UInt(32.W))                     // counter for the number of write address we have stored to
val array_size    = RegInit(0.U(32.W))                // Stores current size of particle filter (n = 10000) in example
val resp_rd = Reg(chiselTypeOf(rocc_rd))                // stores return register
val c_start_addr = Reg(UInt(coreMaxAddrBits.W))         // stores c address for multiply accumulate c += (b x a)
val b_start_addr = Reg(UInt(coreMaxAddrBits.W))         // stores b address for multiply accumulate c += (b x a)
val current_dprv = Reg(chiselTypeOf(rocc_dprv))         // L1 permission register (2b'11) for full access read/write
val current_dv   = Reg(chiselTypeOf(rocc_dv))           // unsure what dv does, typically set to 0 (does this manage non-blocking cases?)                     
val memAddrOffset = RegInit(outer.precision.U(4.W))     // 4.U for 32 bit 8.U for 64 bit)
val addr_tag      = Reg(UInt(2.W))                      // tag of which memory operation this is
val memRespTag = io.mem.resp.bits.tag(1,0)              // return tag of memory operation
val mem_finished    =  Reg(Bool())                      // True we have completed all memory read requests needed for currenter operation
val mem_w_finished  =  Reg(Bool())                      // True we have completed all memory write requests needed for currenter operation                   
val add_finished    =  Reg(Bool())                      // True we have completed all memory additions needed for currenter operation
val div_finished    =  Reg(Bool())                      // True we have completed all memory divisions needed for currenter operation
val mac_finished    =  Reg(Bool())                      // True we have completed all memory macs needed for currenter operation
val exp_finished    =  Reg(Bool())
val div_phase       =  Reg(Bool())
 
val need_writeback  =  Reg(Bool())                      // Do we have data in the write queue that needs to be written back                
val twoSRC          =  Reg(Bool())                      // Are multiple data sources needed for this operation
val src_array       =  Reg(UInt(1.W))
val sumWeights      =  Reg(UInt(64.W))                  // Register to store sum weights after run_sum is completed
val curOp           =  Reg(UInt(2.W))                   // Our current operation 00, 01, 10, 11
val term_1      =  Reg(UInt(65.W))
val term_2      =  Reg(UInt(65.W))
val term_3      =  Reg(UInt(65.W))
val e_likeilood =  Reg(UInt(65.W))
// ==== core interface control signals ==== //
core_cmd.ready := (!rocc_busy)                      // Only ready in idle state
core_response.valid := (rocc_state === r_return)    // Valid return when in return state
io.busy := rocc_busy
// Internal control signals
mem_finished := (counter === array_size && rocc_busy)
mem_w_finished := (write_counter === array_size && rocc_busy)
add_finished := (mem_finished && !i_fifo_1.valid && comp_state =/= c_add && curOp === 1.U && !div_phase)
div_finished := (mem_finished && !i_fifo_1.valid && mem_w_finished && curOp === 1.U)
mac_finished := (mem_finished && !i_fifo_1.valid && !i_fifo_2.valid && comp_state =/= c_mac && curOp === 2.U)
exp_finished := (mem_finished && !i_fifo_1.valid && mem_w_finished && curOp === 3.U)
need_writeback := rocc_interal.out_data_w.valid
// ================================================================ //
// ============ INITALIZE FP HARDWARE ============ //
fpAdder.io.subOp := false.B
fpAdder.io.a := 0.U
fpAdder.io.b := 0.U
fpAdder.io.roundingMode := 0.U       
fpAdder.io.detectTininess := false.B
fpAddSHR := fpAdder.io.exceptionFlags
/*
fpDiv.io.sqrtOp := false.B
fpDiv.io.roundingMode := 0.U       
fpDiv.io.a := 0.U
fpDiv.io.b := 0.U
fpDivSHR := fpDiv.io.exceptionFlags
fpDiv.io.detectTininess := false.B
fpDiv.io.inValid := false.B*/
 
for (i <- 0 until outer.numDivUnits) {
   div_units_io(i).sqrtOp := false.B
   div_units_io(i).roundingMode := 0.U       
   div_units_io(i).a := 0.U
   div_units_io(i).b := 0.U
   div_units_io(i).detectTininess := false.B
   div_units_io(i).inValid := false.B
 
   fpDivSHRs(i) := div_units_io(i).exceptionFlags
}
 
fpMAC.io.op := 0.U
fpMAC.io.a := 0.U
fpMAC.io.b := 0.U
fpMAC.io.c := 0.U
fpMAC.io.roundingMode := 0.U
fpMAC.io.detectTininess := false.B
fpMACSHR := fpMAC.io.exceptionFlags
fpMul.io.a := 0.U
fpMul.io.b := 0.U
fpMul.io.roundingMode := 0.U
fpMul.io.detectTininess := false.B
fpMulSHR := fpMul.io.exceptionFlags
fpMul2.io.a := 0.U
fpMul2.io.b := 0.U
fpMul2.io.roundingMode := 0.U
fpMul2.io.detectTininess := false.B
fpMul2SHR := fpMul.io.exceptionFlags
// =============================================== //
// ============ INITALIZE SO CHISEL DOESN"T COMPLAIN ============ //
rocc_interal.in_data_1.bits := 0.U     
rocc_interal.in_data_2.bits := 0.U     
rocc_interal.in_data_w.bits := 0.U
rocc_interal.in_addr_r.bits := 0.U
rocc_interal.in_addr_w.bits := 0.U
 
rocc_interal.in_data_1.valid := false.B
rocc_interal.in_data_2.valid := false.B
rocc_interal.in_data_w.valid := false.B
rocc_interal.in_addr_r.valid := false.B
rocc_interal.in_addr_w.valid := false.B
 
rocc_interal.out_data_1.ready := false.B
rocc_interal.out_data_2.ready := false.B
rocc_interal.out_data_w.ready := false.B
rocc_interal.out_addr_r.ready := false.B
rocc_interal.out_addr_w.ready := false.B
 
 
io.mem.req.valid := false.B
 
core_response.bits.rd := resp_rd                 
core_response.bits.data := 0.U        
// ============================================================== //
// CO-PROCESSOR STATE
switch(rocc_state) {
 is(r_idle)
 {
     when(core_cmd.fire() && !rocc_busy){
         when(doInit){
            array_size := rocc_rs1
            counter := 0.U
            sumWeights := 0.U
            sum_weights := 0.U
            addr_tag := 0.U
            curOp := 0.U
            current_dprv := rocc_dprv
            current_dv   := rocc_dv
            resp_rd := 0.U
            twoSRC := false.B
            rocc_state := r_idle
            rocc_busy := false.B
         }.elsewhen(doDiv) {
            curr_a_addr := rocc_rs1
            weights_start_addr := rocc_rs1
            current_dprv := rocc_dprv
            current_dv   := rocc_dv
            curr_wb_addr := rocc_rs1
            curOp := 1.U
            counter := 0.U
            write_counter := 0.U
            resp_rd := 0.U
            rocc_state := r_busy
            div_phase := false.B
            rocc_busy := true.B
            twoSRC := false.B
         }.elsewhen(doMAC){
          
             curr_a_addr := rocc_rs1
             curr_b_addr := rocc_rs2
             mac_total := 0.U
             curOp := 2.U
             counter := 0.U
             src_array := 0.U
             current_dprv := rocc_dprv
             current_dv   := rocc_dv
             resp_rd := rocc_rd
             rocc_state := r_busy
             rocc_busy := true.B
             twoSRC := true.B
         }.elsewhen(doExp){
           
             curr_a_addr := rocc_rs1 // address for e^a term (likelihood)
             curr_b_addr := rocc_rs2 // address for b term
             curr_wb_addr := rocc_rs2 // writeback to weights address
             expo_total := 0.U
             curOp := 3.U
             counter := 0.U
             src_array := 0.U
             write_counter := 0.U
             current_dprv := rocc_dprv
             current_dv   := rocc_dv
             resp_rd := rocc_rd
             rocc_state := r_busy
             rocc_busy := true.B
             twoSRC := true.B
          }.otherwise{
           rocc_busy := false.B
         }
     }
 }
 is(r_busy){
 
   when(curOp === 0.U){
     rocc_state := r_idle
   }.elsewhen(div_finished && curOp === 1.U)
   {
     rocc_state := r_idle
     rocc_busy := false.B
   }.elsewhen(mac_finished && curOp === 2.U)
   {
     rocc_state := r_return
   }.elsewhen(exp_finished && curOp === 3.U){
     rocc_state := r_idle
     rocc_busy := false.B
   }.otherwise{
     rocc_state := r_busy
   }
   // Managing adding phase of div op
   when(add_finished && curOp === 1.U){
       div_phase := true.B
       sumWeights := sum_weights
       curr_a_addr := weights_start_addr
       counter := 0.U
   }
 
 }
}
// =========== MEMORY INTERFACE STATE =========== //
switch(mem_state){
is(m_idle){
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
            when(src_array === 0.U)
            { // Pull from source 1 array
                  io.mem.req.valid := rocc_interal.in_data_1.ready
                 io.mem.req.bits.dprv := current_dprv
                 io.mem.req.bits.dv := current_dv
                 io.mem.req.bits.addr := curr_a_addr
                 io.mem.req.bits.tag := addr_tag
                 io.mem.req.bits.cmd := M_XRD // load
                 io.mem.req.bits.size := log2Ceil(outer.precision).U  // 64 bits
                 when(io.mem.req.valid && io.mem.req.ready) { // When we send off the request
                     addr_tag := addr_tag + 1.U // iterate the address we want to access
                     // We only iterate counter when both sources are here
                     mem_state := m_wait // wait for response
                     curr_a_addr := curr_a_addr + memAddrOffset
                 }.otherwise{
                     mem_state := m_stream_read1 // stay in read 1 state
                 }
            }.elsewhen(src_array === 1.U){
             
                 io.mem.req.valid := rocc_interal.in_data_2.ready
                 io.mem.req.bits.dprv := current_dprv
                 io.mem.req.bits.dv := current_dv
                 io.mem.req.bits.addr := curr_b_addr
                 io.mem.req.bits.tag := addr_tag
                 io.mem.req.bits.cmd := M_XRD // load
                 io.mem.req.bits.size := log2Ceil(outer.precision).U  // 64 bits
                 when(io.mem.req.valid && io.mem.req.ready) { // When we send off the request
                     addr_tag := addr_tag + 1.U // iterate the address we want to access
                     counter := counter + 1.U // iterate the counter
                     mem_state := m_wait // wait for response
                     curr_b_addr := curr_b_addr + memAddrOffset
                 }.otherwise{
                     mem_state := m_stream_read1 // stay in read 1 state
                 }
            }
        }
        .otherwise // If one source array
        {
            // send our memory request
            io.mem.req.valid := rocc_interal.in_data_1.ready
            io.mem.req.bits.dprv := current_dprv
            io.mem.req.bits.dv := current_dv
            io.mem.req.bits.addr := curr_a_addr
            io.mem.req.bits.tag := addr_tag
            io.mem.req.bits.cmd := M_XRD // load
            io.mem.req.bits.size := log2Ceil(outer.precision).U  // 64 bits
            when(io.mem.req.valid && io.mem.req.ready) { // When we send off the request
                addr_tag := addr_tag + 1.U // iterate the address we want to access
                counter := counter + 1.U // iterate the counter
                mem_state := m_wait // wait for response
 
                when(curOp === 1.U && div_phase) { // only inqueue addr when needed
                   rocc_interal.in_addr_r.valid := true.B
                   rocc_interal.in_addr_r.bits := curr_a_addr
                }
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
     //rocc_interal.out_data_w.ready := true.B
   // when(!writing && counter < array_size) // When we are not writing and counter is less than the size of the array
   when((write_counter < array_size && curOp =/= 1.U))
    {           
           // send our memory request
            io.mem.req.valid := rocc_interal.out_data_w.valid
            io.mem.req.bits.dprv := current_dprv
            io.mem.req.bits.dv := current_dv
            io.mem.req.bits.addr := curr_wb_addr
            io.mem.req.bits.tag := addr_tag
            io.mem.req.bits.data := rocc_interal.out_data_w.bits
            io.mem.req.bits.cmd := M_XWR // store
            io.mem.req.bits.size := log2Ceil(outer.precision).U  // 64 bits
            when(io.mem.req.valid && io.mem.req.ready)
            { // When we send off the request
                rocc_interal.out_data_w.ready := true.B
                addr_tag := addr_tag + 1.U // iterate the address we want to access
                write_counter := write_counter + 1.U // iterate the counter
                mem_state := m_w_wait // wait for response
                curr_wb_addr := curr_wb_addr + memAddrOffset
            }.otherwise
            {
                mem_state := m_writeback // stay in read 1 state
            }
   }.elsewhen((write_counter < array_size && curOp === 1.U)){
       // send our memory request
            io.mem.req.valid := (rocc_interal.out_data_w.valid && rocc_interal.out_addr_w.valid)
            io.mem.req.bits.dprv := current_dprv
            io.mem.req.bits.dv := current_dv
            io.mem.req.bits.addr := rocc_interal.out_addr_w.bits
            io.mem.req.bits.tag := addr_tag
            io.mem.req.bits.data := rocc_interal.out_data_w.bits
            io.mem.req.bits.cmd := M_XWR // store
            io.mem.req.bits.size := log2Ceil(outer.precision).U  // 64 bits
            when(io.mem.req.valid && io.mem.req.ready)
            { // When we send off the request
                rocc_interal.out_data_w.ready := true.B
                rocc_interal.out_addr_w.ready := true.B
                addr_tag := addr_tag + 1.U // iterate the address we want to access
                write_counter := write_counter + 1.U // iterate the counter
                mem_state := m_w_wait // wait for response
            }.otherwise
            {
                mem_state := m_writeback // stay in read 1 state
            }
   }.otherwise
    {
        mem_state := m_idle
    }
}
is(m_wait){
  io.mem.req.valid := false.B
  when(twoSRC)
 {
     when(io.mem.resp.valid)
     {
          when(src_array === 0.U) {
             rocc_interal.in_data_1.valid := true.B
             rocc_interal.in_data_1.bits := io.mem.resp.bits.data
             mem_state := m_stream_read1
             src_array := 1.U
          }.elsewhen(src_array === 1.U){
             rocc_interal.in_data_2.valid := true.B
             rocc_interal.in_data_2.bits := io.mem.resp.bits.data
             mem_state := m_stream_read1
             src_array := 0.U
          }
     }.otherwise
     {
         mem_state := m_wait
     }
 }.otherwise
 {
     when(io.mem.resp.valid)
     { // If reponse has come in assign incoming data to reg file and goto sum
           rocc_interal.in_data_1.valid := true.B
           rocc_interal.in_data_1.bits := io.mem.resp.bits.data
           mem_state := m_stream_read1
           curr_a_addr := curr_a_addr + memAddrOffset
     }.otherwise {
             mem_state := m_wait
     }
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
var fifo_issued = false.B
var fifo_w_issued = false.B

switch(comp_state){
is(c_idle){
    //val comp_add = (rocc_busy && !add_finished && rocc_interal.out_data_1.valid && curOp === 0.U)
    val comp_div = (rocc_busy && !div_finished && rocc_interal.out_data_1.valid && curOp === 1.U)
    val comp_mac = (rocc_busy && !mac_finished && rocc_interal.out_data_1.valid && rocc_interal.out_data_2.valid && curOp === 2.U)
    val comp_exp = (rocc_busy && !exp_finished && rocc_interal.out_data_1.valid && curOp === 3.U)
    when(comp_div){
       when(div_phase){
           comp_state := c_div
       }.otherwise{
           comp_state := c_add
       }
      
     }.elsewhen(comp_mac){
       comp_state := c_mac
     }.elsewhen(comp_exp){
       comp_state := c_exp
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
// WE CAN ADD MORE DIV UNITS TO INCREASE THROUGH PUT
is(c_div){
   
   
   val need_allocate = rocc_interal.out_data_1.valid && rocc_interal.out_addr_r.valid
   val can_write = rocc_interal.in_data_w.ready && rocc_interal.in_addr_w.ready
 
   when(need_allocate)
   {
     var allocated = false.B
       for (i <- 0 until outer.numDivUnits)
       {
           val can_allocate = div_occupied(i) =/= 1.U
 
           when(!allocated && can_allocate && !fifo_issued)
           {
              div_units_io(i).inValid := true.B
              rocc_interal.out_data_1.ready := true.B
              rocc_interal.out_addr_r.ready := true.B

              div_occupied(i) := 1.U
              div_units_io(i).a := recFNFromFN(outer.expWidth, outer.sigWidth, rocc_interal.out_data_1.bits)
              div_units_io(i).b := recFNFromFN(outer.expWidth, outer.sigWidth, sumWeights)
              register_file(i)  := rocc_interal.out_addr_r.bits
              //allocated = true.B
           }
           val was_fifo_issued = fifo_issued
           fifo_issued = (can_allocate && !allocated) | fifo_issued
           allocated = (can_allocate && !was_fifo_issued) | allocated
       }
   }
   when(!mem_w_finished){
   var write_queued = false.B
   for (i <- 0 until outer.numDivUnits)
       {
           val need_write = div_w_ready(i) =/= 0.U
           when(div_units_io(i).outValid_div)
           {
               w_reg_file(i) := fNFromRecFN(outer.expWidth, outer.sigWidth, div_units_io(i).out)
               div_w_ready(i) := 1.U
           }
 
           when(need_write && !write_queued && can_write && !fifo_w_issued)
           {
               //write_queued = true.B
               rocc_interal.in_data_w.valid := true.B
               rocc_interal.in_addr_w.valid := true.B
 
               rocc_interal.in_data_w.bits := w_reg_file(i)
               rocc_interal.in_addr_w.bits := register_file(i)
               div_occupied(i) := 0.U
               div_w_ready(i) := 0.U
           }
           val was_wfifo_issued = fifo_w_issued
           fifo_w_issued = (can_write && !write_queued && need_write) | fifo_w_issued
           write_queued = (can_write && !was_wfifo_issued && need_write) | write_queued
       }
   }.otherwise{
       comp_state := c_idle
   }
 
}
/*is(c_wait_div)
{
  when(fpDiv.io.outValid_div){
     rocc_interal.in_data_w.valid := true.B
     rocc_interal.in_data_w.bits := fNFromRecFN(outer.expWidth, outer.sigWidth, fpDiv.io.out)
     when(rocc_interal.out_data_1.valid){
       comp_state := c_div
     }.otherwise{
       comp_state := c_idle
     }
  }.otherwise{
    comp_state := c_wait_div
  }
}*/
is(c_mac){
    rocc_interal.out_data_1.ready := true.B
    rocc_interal.out_data_2.ready := true.B
    when(rocc_interal.out_data_1.fire() && rocc_interal.out_data_2.fire()){  // This may cause issues if we enter this state without out1 and out2 being valid
         fpMAC.io.c := recFNFromFN(outer.expWidth, outer.sigWidth, mac_total)
         fpMAC.io.a := recFNFromFN(outer.expWidth, outer.sigWidth, rocc_interal.out_data_1.bits)
         fpMAC.io.b := recFNFromFN(outer.expWidth, outer.sigWidth, rocc_interal.out_data_2.bits)
         mac_total :=  fNFromRecFN(outer.expWidth, outer.sigWidth,fpMAC.io.out)
    }.otherwise{
        comp_state := c_idle
    }
}
is(c_exp){
    rocc_interal.out_data_1.ready := true.B
    when(rocc_interal.out_data_1.fire())
     {
         // y + 1
         fpAdder.io.a := recFNFromFN(outer.expWidth, outer.sigWidth, rocc_interal.out_data_1.bits)
         fpAdder.io.b := recFNFromFN(outer.expWidth, outer.sigWidth, expValues(0))
         term_1 := fpAdder.io.out // Again in recorded FP format
         // y^2
         fpMul.io.a := recFNFromFN(outer.expWidth, outer.sigWidth, rocc_interal.out_data_1.bits)
         fpMul.io.b := recFNFromFN(outer.expWidth, outer.sigWidth, rocc_interal.out_data_1.bits)
         term_2 := fpMul.io.out // 2nd term in recorded FP format     (latch to decouple stages)    
         // y * 1/3!
         fpMul2.io.a := recFNFromFN(outer.expWidth, outer.sigWidth, rocc_interal.out_data_1.bits)
         fpMul2.io.b := recFNFromFN(outer.expWidth, outer.sigWidth, expValues(3))
         term_3 := fpMul2.io.out // 3rd term
         comp_state := c_exp2
     }
}
is(c_exp2){ // Transitory stage
      // y + 1 + (y^2 * 1/2!)
      // (term 1) + (term2 * exp[2])
      fpMAC.io.c := term_1
      fpMAC.io.a := term_2
      fpMAC.io.b := recFNFromFN(outer.expWidth, outer.sigWidth, expValues(2))
      term_2 := fpMAC.io.out     
      // y^3 * 1/3!
      fpMul.io.a := term_2
      fpMul.io.b := term_3
      term_3 := fpMul.io.out
    
      comp_state := c_exp3
     }
 is(c_exp3){
     fpAdder.io.a := term_2
     fpAdder.io.b := term_3
     e_likeilood := fpAdder.io.out
     comp_state := c_exp_w
 }
 is(c_exp_w){
  when(rocc_interal.in_data_w.ready)
  {
      rocc_interal.out_data_2.ready := true.B
      when(rocc_interal.out_data_2.fire())
      {
          rocc_interal.in_data_w.valid := true.B
          fpMul.io.a := e_likeilood
          fpMul.io.b := recFNFromFN(outer.expWidth, outer.sigWidth, rocc_interal.out_data_2.bits)
          rocc_interal.in_data_w.bits := fNFromRecFN(outer.expWidth, outer.sigWidth, fpMul.io.out)
          when(rocc_interal.out_data_1.valid)
            {
              comp_state := c_exp
            }.otherwise{
              comp_state := c_idle
            }
      }
  }.otherwise{
    comp_state := c_exp_w
  }
}
}
when (core_response.fire()) {
 rocc_state := r_idle
 /*when(curOp === 0.U)
 {
     sumWeights := sum_weights // Set internal register to completed sum for use later
     core_response.bits.data := sum_weights
 }*/
 when(curOp === 2.U)
 {
     core_response.bits.data := mac_total
 }
 rocc_busy := false.B
}
// ====== CORE INTERFACE UNUSED ====== //
io.interrupt := false.B // We do not need to interrupt the processor
// =================================== //
// =========================  MEMORY REQUEST UNUSED =========================== //
io.mem.req.bits.signed := false.B   // Should not need
io.mem.req.bits.phys   := false.B   // All acceses are virtual and should be translated
// ========================================================+++++++============= //
}
