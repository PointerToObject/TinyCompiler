# Aiden Compiler

## Overview

Aiden Compiler is a minimal C-like compiler that translates a subset of C code into 32-bit NASM assembly. It is designed for low-level programming, including kernel development and direct hardware manipulation. The compiler supports basic integer arithmetic, pointers to integers, and simple output to VGA text memory.

This compiler is intended for educational purposes and for experimenting with bare-metal programming.

---

## Supported Language Features

* **Data Types**

  * `int` (32-bit integers)
  * `int*` (pointers to integers)

* **Variable Assignment**

  * Example: `int a = 42;`

* **Arithmetic**

  * Addition only: `int c = a + b;`

* **Pointers**

  * Declaring integer pointers: `int* ptr = &a;`
  * Dereferencing pointers: `*ptr = 10;`

* **Statements**

  * `return <expression>;`
  * `print(<expression>);` — writes a character to VGA text memory

* **Limitations**

  * No control flow (`if`, `while`, `for`)
  * No functions besides `kernel_main`
  * Only supports integer expressions
  * No advanced data structures

---

## Usage

The compiler is a console application and is designed to run from the **Visual Studio Developer Command Prompt** or any terminal with proper path access.

### Steps:

1. Navigate to the directory containing your C source code and the compiler:

```cmd
cd /d "D:\OSDev\AidenOS\AidenOS\Kernel"
```

2. Run the compiler with the following command:

```cmd
TestCompiler.exe kernel.c kernel.asm
```

* `kernel.c` is the input C source file.
* `kernel.asm` is the output NASM assembly file.

3. Assemble and link the generated assembly file using NASM:

```cmd
nasm -f bin kernel.asm -o kernel.bin
```

4. Combine with your bootloader to produce a bootable image, or run in QEMU for testing:

```cmd
qemu-system-x86_64 -drive format=raw,file=boot.img
```

---

## Example

```c
int kernel_main() {
    int a = 69;
    int b = 420;
    int c = a + b;
    int* ptr = &c;
    *ptr = 69420;

    return *ptr;
}
```

Generated assembly (partial):

```asm
[BITS 32]
section .bss
var0 resd 1
var1 resd 1
var2 resd 1

section .text
global kernel_main
kernel_main:
    mov eax, 69
    mov [var0], eax ; int a
    mov eax, 420
    mov [var1], eax ; int b
    mov eax, [var0]
    push eax
    mov eax, [var1]
    pop ebx
    add eax, ebx
    mov [var2], eax ; int c
    lea eax, [var2]
    mov [var1], eax ; int* ptr
    mov eax, 22
    push eax
    mov eax, [var1]
    pop ebx
    mov [eax], ebx
    mov eax, [var1]
    mov eax, [eax]
    ret
```

---

## Notes

* This compiler produces **flat 32-bit NASM assembly** suitable for direct integration into bootloader-based projects.
* All variables are stored in the `.bss` section.
* Each statement is explicitly represented in assembly; there is no optimization.
* VGA output is done by writing directly to memory address `0xB8000`.

---

## License

This compiler is provided as-is for educational and experimental purposes.
