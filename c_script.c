#include <stdio.h>
#include "platform.h"
#include "xparameters.h"
#include "sleep.h"
#include <inttypes.h> //to set 32 bit output
#include "xil_types.h"
#include "xil_io.h"
#include "platform.h"
#include "xil_printf.h"
#include "xparameters.h"
#include "sleep.h"
#define MY_IP_BASEADDR 0x43C00000


int main() {
    init_platform();
    unsigned int A_ip, data_in_ip, valid_in_ip, clk_ip, rst_ip; // inputs
    unsigned int valid_out_ip, y_ip, B_ip; // outputs
//    xil_printf("start");
//    A_ip = 0xffffffff;
//    xil_printf("A is: %d\n",A_ip);
//    Xil_Out32((MY_IP_BASEADDR+0x00),A_ip);
//    usleep(500);
//
//    B_ip = Xil_In32(MY_IP_BASEADDR + 0x04);
//    xil_printf("B2 is: %d\n",B_ip);


    rst_ip = 1;
    data_in_ip = 0;
    valid_in_ip = 0;
    rst_ip = rst_ip << 9;
    valid_in_ip = valid_in_ip << 8;
    A_ip = rst_ip | valid_in_ip | data_in_ip;
    xil_printf("A_initialize is: %d\n",A_ip);

    Xil_Out32((MY_IP_BASEADDR+0x00),A_ip);
    unsigned int data[12]={112, 97, 195, 203, 47, 125, 114, 165, 181, 193, 70, 174};
    //unsigned int data[12]={1,2,3,4,5,6,7,8};

    for (int i=0; i<12; i++) {
		rst_ip = 0;
		data_in_ip = data[i];
		valid_in_ip = 1;
		rst_ip = rst_ip << 9;
		valid_in_ip = valid_in_ip << 8;
		A_ip = 0 | rst_ip | valid_in_ip | data_in_ip;
		unsigned input = A_ip - 256;
		xil_printf("A_input %d is: %d\n", i,input);
		Xil_Out32((MY_IP_BASEADDR+0x00),A_ip);
		//usleep(1000);
		do {
			B_ip = Xil_In32(MY_IP_BASEADDR + 0x04);
		}
		while ((B_ip & 0x00020000) == 0x00000000);
		unsigned output = B_ip-131072;
	    xil_printf("B_output %d is: %d\n\n", i, output);
    }

//    rst_ip = 0;
//    data_in_ip = 20;
//    valid_in_ip = 1;
//    rst_ip = rst_ip << 9;
//    valid_in_ip = valid_in_ip << 8;
//    A_ip = rst_ip | valid_in_ip | data_in_ip;
//    xil_printf("A3 is: %d\n",A_ip);
//
//    Xil_Out32((MY_IP_BASEADDR+0x00),A_ip);
//    usleep(500);
//    do {
//        B_ip = Xil_In32(MY_IP_BASEADDR + 0x04);
////        xil_printf("waiting...\n");
//    } //while ((B_ip & 0x00000001) == 1);
//    while ((B_ip & 0x00010000) == 1);
//
//    xil_printf("B2 is: %d\n",B_ip);
    cleanup_platform();
    return 0;
}
