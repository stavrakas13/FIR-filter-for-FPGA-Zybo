# Analytical Overview: FIR Filter AXI4Lite System

## Overview

This project implements a Finite Impulse Response (FIR) filter in VHDL, interfaced with a host processor via an AXI4Lite bus. The system consists of a hardware design and a C test script that demonstrates how to interact with the hardware.

Implemented by Ioannis Danias and me on XILINX VIVADO 2018.3

---

## 1. VHDL Design (`final_axi.vhdl`)

### Top-Level Structure

- **AXI4Lite Slave Interface**: Handles communication with the host processor, mapping registers for input and output.
- **FIR Filter Core**: Implements the FIR logic using submodules:
  - `control_unit`: Manages control signals and address generation.
  - `mlab_ram`: Stores input samples.
  - `mlab_rom`: Stores filter coefficients.
  - `MAC`: Performs multiply-accumulate operations.

### Data Flow

1. **Input Handling**:  
   - The host writes input data and control signals (reset, valid) to the mapped AXI register (`slv_reg0`).
   - The FIR core receives these signals and processes the input sample.

2. **Processing**:  
   - On a valid input, the control unit resets counters and triggers a write to RAM.
   - The RAM shifts in the new sample; the ROM provides the corresponding coefficient.
   - The MAC unit multiplies the RAM and ROM outputs, accumulating the result.

3. **Output Handling**:  
   - When processing is complete, the output (`y_ip`) and a valid flag (`valid_out_ip`) are packed into the output register (`B_ip`).
   - The host reads the result from the mapped AXI register.

### Register Map

- **slv_reg0**: Input register (data, valid, reset).
- **B_ip**: Output register (result, valid flag).

---

## 2. C Script (`c_script.c`)

### Purpose

Demonstrates how to interact with the FIR hardware via memory-mapped AXI registers.

### Logic

1. **Initialization**:
   - Platform is initialized.
   - The FIR core is reset by writing to the input register.

2. **Input Loop**:
   - For each sample in the input array:
     - Compose the input word (data, valid, reset).
     - Write to the FIR input register.
     - Poll the output register until the valid flag is set.
     - Read and print the output.

3. **Bit Packing**:
   - Input word:  
     - Bits 0-7: Data sample  
     - Bit 8: Valid flag  
     - Bit 9: Reset flag  
   - Output word:  
     - Bits 0-16: FIR result  
     - Bit 17: Output valid flag

### Key Interactions

- **Write**: `Xil_Out32(MY_IP_BASEADDR+0x00, A_ip);`
- **Read**: `B_ip = Xil_In32(MY_IP_BASEADDR + 0x04);`
- **Polling**: Waits for the valid flag in the output register before reading the result.

---

## 3. System Logic Summary

- The host writes input data and control signals to the FIR core via AXI registers.
- The FIR core processes the data, using RAM for sample storage and ROM for coefficients.
- The MAC unit computes the FIR output.
- The result and a valid flag are made available to the host, which reads them after polling for completion.

---

## 4. Notes

- The design is synchronous and expects the host to manage valid and reset signals.
- The C script demonstrates a typical usage pattern: reset, input, poll for output, read result.
- The VHDL code is modular, allowing for easy adaptation of filter length and coefficients.

---
