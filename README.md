# Aiden Compiler Documentation

**Version 1.0**  
**Language:** Aiden++  
**Target Architecture:** x86 (32-bit)  
**Author:** [Your Name]

---

## Overview

**Aiden Compiler** is a specialized C-to-assembly compiler designed for bare-metal x86 operating system development. It compiles **Aiden++**, a C dialect optimized for kernel and bootloader programming, directly to 32-bit x86 NASM assembly language without requiring any runtime libraries or operating system support.

### Key Features

- **Zero-Dependency Architecture** - Generates self-contained code with no external runtime requirements
- **Direct Hardware Access** - First-class support for inline assembly and memory-mapped I/O
- **Comprehensive Language Support** - Preprocessor directives, floating-point arithmetic, structures, pointers, and arrays
- **IDE Integration Ready** - Designed for seamless integration with the upcoming Aiden IDE
- **VGA Text Output** - Built-in print function for kernel debugging and display

### Technical Specifications

| Specification | Value |
|--------------|-------|
| Target Architecture | x86 (IA-32) |
| Bit Mode | 32-bit protected mode |
| Output Format | NASM Assembly (.asm) |
| Memory Model | Flat (origin 0x1000) |
| Calling Convention | Stack-based (cdecl-compatible) |
| Floating Point | IEEE 754 single-precision |

---

## Getting Started

### Building the Compiler
```bash
gcc -o aiden compiler.c -std=c99 -O2
```

### Basic Usage
```bash
# Compile Aiden++ source to assembly
./aiden kernel.c kernel.asm

# Assemble to binary
nasm -f bin kernel.asm -o kernel.bin

# Run in QEMU
qemu-system-i386 -kernel kernel.bin
```

### Your First Program
```c
int kernel_main() {
    print("Hello from Aiden++\n");
    return 0;
}
```

---

## Language Reference

### Data Types

| Type | Size | Range | Description |
|------|------|-------|-------------|
| `int` | 4 bytes | ±2.1 billion | 32-bit signed integer |
| `__int16` | 2 bytes | ±32,768 | 16-bit signed integer |
| `__int8` | 1 byte | ±128 | 8-bit signed integer |
| `char` | 1 byte | ±128 | Character/byte value |
| `float` | 4 bytes | ~7 digits | IEEE 754 single-precision |
| `void` | - | - | No return value |
| `T*` | 4 bytes | - | Pointer to type T |

### Preprocessor
```c
// Macro definitions
#define MAX_SIZE 1024
#define VGA_ADDR 0xB8000

// File inclusion
#include "drivers.h"
#include "memory.h"
```

### Operators

**Arithmetic:** `+` `-` `*` `/` `++` `--`  
**Comparison:** `==` `!=` `<` `>` `<=` `>=`  
**Bitwise:** `<<` `>>` `|` `&`  
**Pointer:** `&` (address-of) `*` (dereference)  
**Assignment:** `=`

### Control Flow
```c
// If statement
if (x > 10) {
    print("Large value\n");
}

// While loop
int i = 0;
while (i < 10) {
    print("Count: %d\n", i);
    ++i;
}

// Break statement
while (1) {
    if (done) break;
}
```

### Functions
```c
int add(int a, int b) {
    return a + b;
}

void initialize() {
    print("System ready\n");
}

int kernel_main() {
    int sum = add(10, 20);
    initialize();
    return 0;
}
```

### Pointers
```c
int value = 42;
int* ptr = &value;
*ptr = 100;              // value is now 100

// Direct memory access
int* vga = 0xB8000;
*vga = 0x0F41;           // Write 'A' to screen
```

### Arrays
```c
int data[10];
data[0] = 100;
data[1] = 200;

int i = 0;
while (i < 10) {
    data[i] = i * 10;
    ++i;
}
```

### Structures
```c
struct Point {
    int x;
    int y;
};

struct Point p = {10, 20};
p.x = 30;

struct Point* ptr = &p;
ptr->y = 40;
```

### Inline Assembly
```c
__asm__("cli");              // Clear interrupts
__asm__("hlt");              // Halt processor
__asm__("mov eax, 0x1000");  // Register manipulation
```

### Built-in Print Function
```c
print("Hello, World\n");
print("Value: %d\n", 42);
print("Hex: 0x%x\n", 0xFF);
print("Char: %c\n", 'A');
print("Float: %f\n", 3.14159);
```

**Format Specifiers:**
- `%d` - Decimal integer
- `%x` - Hexadecimal (8 digits)
- `%c` - Single character
- `%f` - Floating-point (6 decimal places)

---

## Complete Examples

### VGA Text Output
```c
int kernel_main() {
    int* vga = 0xB8000;
    vga[0] = 0x0F41;  // White 'A'
    vga[1] = 0x0F42;  // White 'B'
    vga[2] = 0x0F43;  // White 'C'
    return 0;
}
```

### Structure with Pointers
```c
struct Registers {
    int eax;
    int ebx;
    int ecx;
};

void save_state(struct Registers* regs) {
    regs->eax = 0;
    regs->ebx = 0;
    regs->ecx = 0;
}

int kernel_main() {
    struct Registers state;
    save_state(&state);
    print("State saved\n");
    return 0;
}
```

### Floating-Point Math
```c
#define PI 3.14159

float calculate_area(float radius) {
    return PI * radius * radius;
}

int kernel_main() {
    float r = 5.0;
    float area = calculate_area(r);
    print("Area: %f\n", area);
    return 0;
}
```

### Hardware Port I/O
```c
void write_port(int port, char value) {
    __asm__("mov edx, [port]");
    __asm__("mov al, [value]");
    __asm__("out dx, al");
}

int kernel_main() {
    write_port(0x3F8, 'H');
    return 0;
}
```

### Array Processing
```c
int kernel_main() {
    int buffer[16];
    int i = 0;
    
    while (i < 16) {
        buffer[i] = i * 10;
        ++i;
    }
    
    i = 0;
    while (i < 16) {
        print("buffer[%d] = %d\n", i, buffer[i]);
        ++i;
    }
    
    return 0;
}
```

---

## IDE Integration

The Aiden Compiler is designed for seamless integration with the upcoming **Aiden IDE**, a complete development environment for operating systems and bootloaders.

### Planned Features

- **Project Templates** - Pre-configured kernel and bootloader projects
- **One-Click Build** - Automatic compilation, assembly, and linking
- **Integrated Emulation** - QEMU integration with debug output capture
- **Syntax Highlighting** - Full Aiden++ language support
- **Code Completion** - Context-aware suggestions for functions and structures
- **Hardware Support** - Driver templates and port I/O snippets
- **Real-Time Diagnostics** - Inline error detection and suggestions

### Workflow

1. Create new kernel project from template
2. Edit source with syntax highlighting
3. Press F5 to compile and run in emulator
4. View output in integrated console
5. Debug with memory and register inspection

---

## Limitations

**Unsupported Features:**
- Unsigned types (planned for next release)
- Type casting (planned for next release)
- Unions (planned for next release)
- for loops (use while instead)
- switch/case statements
- Function pointers
- Global variables
- Compound assignment operators (+=, -=, etc.)

**Memory Constraints:**
- Maximum 256 variables per function
- Maximum 8192 tokens per compilation unit
- Maximum 256 string/float literals
- Maximum 64 structure definitions

---

## Technical Details

### Memory Layout

- **Code Origin:** 0x1000
- **VGA Buffer:** 0xB8000 (80x25 text mode)
- **Variable Storage:** BSS section (static allocation)

### Assembly Output Structure
```nasm
[BITS 32]
[org 0x1000]

section .text
    ; Function code

section .data
    ; String and float literals

section .bss
    ; Variable storage
```

### Calling Convention

- Parameters pushed right-to-left
- Caller cleans up stack
- Return values in EAX (integers) or FPU ST(0) (floats)
- Stack frame only for kernel_main

---

## Roadmap

**Version 1.1 (Planned)**
- Unsigned integer types (unsigned int, unsigned char)
- Type casting operations
- Union data type
- Port I/O built-in functions (outb, inb, outw, inw)
- Volatile keyword for MMIO

**Version 1.2 (Planned)**
- sizeof operator
- Enumerations (enum)
- Function pointers
- Static variables
- Switch/case statements

**Version 2.0 (Planned)**
- Aiden IDE release
- Enhanced optimization
- Inline function support
- Advanced preprocessor features

---

## System Requirements

**Compiler:**
- C99-compliant compiler (GCC 4.8+ or Clang 3.4+)
- 64 MB RAM minimum

**Target:**
- x86 processor (i386+)
- NASM 2.10+
- GNU LD (binutils)
- QEMU, Bochs, VirtualBox, or real hardware

---

## License & Support

**Aiden Compiler** - A specialized compiler for bare-metal x86 development  
**Author:** PointerToObject  
**Version:** 1.0  
**License:** DeezNutz


---

*Build operating systems the easy way with Aiden++ and Aiden IDE.*
