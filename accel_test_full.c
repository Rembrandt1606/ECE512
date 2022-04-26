#include <stdio.h>
#include <sys/time.h>
#include <time.h>
#include <stdlib.h>
#include "rocc.h"

static inline int mem_cumsum_rocc_test(uintptr_t *start_ptr, int len)
{
	double value;
	asm volatile ("fence");
	//#define ROCC_INSTRUCTION_DSS(X, rd, rs1, rs2, funct) 
	ROCC_INSTRUCTION_DSS(0, value, (uintptr_t) start_ptr, len, 0);
	return value;
}

static inline int mem_div_rocc_test(uintptr_t *start_ptr)
{
	asm volatile ("fence");
	ROCC_INSTRUCTION_S(0, (uintptr_t) start_ptr, 1);
}

static inline int mem_mac_rocc_test(uintptr_t *a_ptr, uintptr_t *b_ptr)
{
	double mac_total;
	asm volatile("fence");
	ROCC_INSTRUCTION_DSS(0, mac_total, (uintptr_t) a_ptr, (uintptr_t) b_ptr, 2);
	return mac_total;

}

static inline int mem_exp_rocc_test(uintptr_t *like_ptr, uintptr_t *weight_ptr)
{
	asm volatile ("fence"); // need previous memory ops to have finished as we are loading
	ROCC_INSTRUCTION_SS(0, (uintptr_t) like_ptr, (uintptr_t) weight_ptr, 3);
}

float elapsed_time(long long start, long long end) {
	return (float)(end - start) / (1000 * 1000);
}

long long get_time() {
	struct timeval tv;
	gettimeofday(&tv, NULL);
	return(tv.tv_sec * 1000000) + tv.tv_usec;

}

int size = 100;

int main(void)
{
	//srand((unsigned int)time(NULL));
	
	
	double xPos[size];
	double likelihood[size];
	double weights[size];
	
	// using rand on large n breaks SPIKE.pk
	for (int i=0;i<size;i++){
		xPos[i] = (double)(i+1); //(double)(rand()%20);
		likelihood[i] = (double)(i)/(double)(i+3); //((double)rand()/(double)(RAND_MAX));
		weights[i] = (double)(i)/(double)(i+4); //((double)rand()/(double)(RAND_MAX));
	}
	
	uintptr_t *loc_ptr = (uintptr_t *)(&xPos);
	uintptr_t *likeihood_ptr = (uintptr_t *)(&likelihood);
	uintptr_t *weights_ptr = (uintptr_t *)(&weights);
	printf("\n");
	printf("Starting likeihood test ... \n");
	long long exp_time = get_time();
	mem_exp_rocc_test(likeihood_ptr, weights_ptr);
	long long end_exp = get_time();
	printf("Likeihood test done \n");
	printf("Likeihood test took: %f\n", elapsed_time(exp_time, end_exp));
	
	printf("Starting weights run sum ... \n");
	long long add_time = get_time();
	double result = mem_cumsum_rocc_test(weights_ptr, size);
	long long end_add = get_time();
	printf("Weights run sum test done \n");
	printf("Weights test took: %f\n", elapsed_time(add_time, end_add));
		
	printf("Starting weights normalized test ... \n");
	long long div_time = get_time();
	mem_div_rocc_test(weights_ptr);
	long long end_div = get_time(); 
	printf("Normalization test done \n");
	printf("Normalization test Took: %f\n", elapsed_time(div_time, end_div));
		
	printf("Starting Move Test ... \n");
	long long mac_time = get_time();
	double mac_return = mem_mac_rocc_test(weights_ptr, loc_ptr);
	long long end_mac = get_time();
	printf("Move Test Done \n");
	printf("Move Test Took: %f\n", elapsed_time(mac_time, end_mac));
	printf("TEST DONE \n");
	return 0;
}
