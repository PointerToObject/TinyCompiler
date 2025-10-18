# AidenOS Minimal C Compiler

## Overview

AidenOS Minimal C Compiler is a small, educational compiler designed to compile basic C programs for the x86 architecture. It allows users to write simple C code and generate corresponding assembly that runs directly in a bare-metal x86 environment.

This compiler is intended for learning purposes and for experimenting with low-level OS development.

## Features

* Compile basic C syntax including variables, arithmetic, and control structures.
* Output x86 assembly code compatible with NASM.
* Integrates with a custom bootloader for bare-metal execution.
* Minimal dependencies, designed for use in AidenOS development.

## Usage

### Building a C Program

1. Write your C code and save it as `program.c`.

2. Run the compiler to generate assembly:

   ```bash
   ./aidencc program.c
   ```

   **Visual Studio Developer Command Prompt:**

   ```cmd
   aidencc.exe program.c
   ```

3. Assemble the output with NASM:

   ```bash
   nasm -f bin program.asm -o program.bin
   ```

   **Visual Studio Developer Command Prompt:**

   ```cmd
   nasm.exe -f bin program.asm
   ```

4. Copy the binary to your bootloader or ISO root:

   ```bash
   cp program.bin iso_root/
   ```

   **Visual Studio Developer Command Prompt:**

   ```cmd
   copy program.bin iso_root\
   ```

### Running in AidenOS

1. Include the compiled program in your AidenOS build.
2. Boot the OS in an emulator or on real hardware.
3. The program will execute directly on bare metal.

## Limitations

* Only supports a subset of C (no structs, floating-point, or complex libraries).
* Debugging is manual through printing to VGA text memory.
* No standard library support beyond minimal routines.



