# Aiden Compiler Documentation

## Overview

**Aiden Compiler** is a lightweight C-to-x86 assembly compiler designed for bare-metal 32-bit environments. It compiles **Aiden++**, a subset of C with extended features, to NASM-compatible assembly code, targeting VGA text mode output for operating system development and low-level programming.

**Current Version:** 1.0  
**Language:** Aiden++  
**Target Architecture:** x86 (32-bit)  
**Output Format:** NASM Assembly  
**Primary Use Case:** Operating System Kernels & Bare Metal Programming

---

## Getting Started

### Building the Compiler

```bash
gcc compiler.c -o aiden
```

### Using the Compiler

```bash
./aiden input.c output.asm
nasm -f bin output.asm -o output.bin
```

The Aiden Compiler reads Aiden++ source code and generates x86 assembly suitable for bare-metal execution at memory address `0x1000`.

---

## Aiden++ Language Reference

### Data Types

Aiden++ supports the following data types:

| Type | Size | Description |
|------|------|-------------|
| `int` | 4 bytes | Standard 32-bit integer |
| `__int8` | 1 byte | 8-bit integer |
| `__int16` | 2 bytes | 16-bit integer |
| `char` | 1 byte | Character/byte value |
| `void` | 0 bytes | No return value |
| `struct` | Variable | User-defined structure |
| `T*` | 4 bytes | Pointer to type T |

### Numeric Literals

Aiden++ supports multiple number formats:

- **Decimal:** `255`, `42`, `-10`
- **Hexadecimal:** `0xFF`, `0x10`, `0xDEADBEEF`
- **Character:** `'A'`, `'z'`, `'0'`

**Example:**
```c
int decimal = 255;
int hex = 0xFF;        // Same as 255
int negative = -42;
char letter = 'A';     // ASCII 65
```

### Operators

#### Arithmetic Operators
- `+` Addition
- `-` Subtraction
- `*` Multiplication
- `/` Integer division
- `++` Increment (prefix: `++x`)
- `--` Decrement (prefix: `--x`)

#### Bitwise Operators
- `<<` Left shift
- `>>` Right shift (logical)
- `|` Bitwise OR
- `&` Bitwise AND

**Example:**
```c
int flags = 0x0F;
int shifted = flags >> 4;    // 0x0F becomes 0x00
int masked = flags & 0x07;   // Extract lower 3 bits
int combined = flags | 0xF0; // Set upper nibble
```

#### Comparison Operators
- `==` Equal to
- `!=` Not equal to
- `<` Less than
- `>` Greater than
- `<=` Less than or equal to
- `>=` Greater than or equal to

#### Pointer Operators
- `&` Address-of operator
- `*` Dereference operator

---

## Structures

Structures in Aiden++ allow you to create composite data types with multiple fields.

### Defining Structures

```c
struct Point {
    int x;
    int y;
};

struct VGA_Char {
    char c;
    char color;
};

struct Rect {
    __int16 left;
    __int16 top;
    __int16 width;
    __int16 height;
};
```

### Declaring and Initializing Structures

```c
void kernel_main() {
    // Declaration with initialization
    struct Point p = {10, 20};
    struct VGA_Char ch = {'A', 0x0F};
    
    // Declaration without initialization
    struct Rect r;
}
```

### Accessing Structure Fields

```c
void kernel_main() {
    struct Point p = {10, 20};
    
    // Direct access with dot operator
    p.x = 15;
    int y_value = p.y;
    
    // Pointer access with arrow operator
    struct Point* ptr = &p;
    ptr->x = 25;
    ptr->y = 30;
}
```

### Structure Features
- **Ordered field layout** with automatic offset calculation
- Support for **nested types** (primitives and pointers)
- **Field access** via `.` operator for direct access
- **Pointer field access** via `->` operator
- **Aggregate initialization** with brace syntax: `{value1, value2, ...}`
- Fields must be initialized in **declaration order**

---

## Functions

### Function Declaration

```c
void print_hello() {
    print("Hello, World!\n");
}

int add(int a, int b) {
    return a + b;
}

int multiply(int x, int y) {
    return x * y;
}
```

### Calling Functions

```c
void kernel_main() {
    int result = add(5, 10);
    print("Result: %d\n", result);
    print_hello();
}
```

### Function Characteristics
- **Parameters** passed via stack (cdecl convention)
- **`kernel_main`** receives special prologue/epilogue with stack frame setup
- **Return values** placed in `eax` register
- **Automatic return** statement generation if omitted
- **Recursion** supported

---

## Control Flow

### If Statements

Execute code conditionally based on boolean expressions.

```c
if (x == 5) {
    y = 10;
}

if (count > 100) {
    count = 0;
}

// Single statement (braces optional)
if (flag)
    result = 1;
```

**Note:** `else` clause not supported in current version.

### While Loops

Repeat code while a condition is true.

```c
int i = 0;
while (i < 10) {
    print("Count: %d\n", i);
    i++;
}

// Infinite loop
while (1) {
    // Loop forever
}
```

### Break Statement

Exit a loop early.

```c
int i = 0;
while (1) {
    if (i >= 10) {
        break;
    }
    i++;
}
```

---

## Pointers

Pointers in Aiden++ allow direct memory manipulation, essential for bare-metal programming.

### Basic Pointer Usage

```c
void kernel_main() {
    int x = 42;
    int* ptr = &x;      // Get address of x
    *ptr = 100;         // Dereference and assign (x is now 100)
    
    print("Value: %d\n", x);  // Prints: Value: 100
}
```

### Pointer to Structures

```c
struct Point {
    int x;
    int y;
};

void kernel_main() {
    struct Point p = {10, 20};
    struct Point* ptr = &p;
    
    ptr->x = 30;  // Access via arrow operator
    ptr->y = 40;
}
```

### Direct Memory Access

```c
void kernel_main() {
    char* vga = 0xB8000;  // VGA text buffer address
    *vga = 'A';           // Write character to screen
    *(vga + 1) = 0x0F;    // Set color attribute
}
```

### Pointer Arithmetic
```c
char* buffer = 0xB8000;
buffer++;              // Advance by sizeof(char) = 1 byte
int* array = 0x2000;
array++;               // Advance by sizeof(int) = 4 bytes
```

---

## Built-in Print Function

Aiden++ provides a built-in `print()` function for VGA text mode output at address `0xB8000`.

### Syntax

```c
print(format_string);
print(format_string, arg1, arg2, ...);
```

### Format Specifiers

| Specifier | Type | Output | Example |
|-----------|------|--------|---------|
| `%c` | char | Single character | `'A'` → `A` |
| `%d` | int | Decimal integer | `42` → `42` |
| `%x` | int | 8-digit hexadecimal | `255` → `000000FF` |

### Examples

```c
// Simple string
print("Hello World\n");

// Integer formatting
print("Value: %d\n", 42);

// Hexadecimal formatting
print("Hex: 0x%x\n", 0xFF);

// Character formatting
print("Char: %c\n", 'A');

// Multiple arguments
print("Char: %c, Val: 0x%x, Shift: %d\n", 'Z', 0xFF, 15);

// Structure field printing
struct VGA_Char ch = {'A', 0x0F};
int x = 0xFF;
print("Char: %c, Val: 0x%x, Shift: %d\n", ch.c, x, x >> 4);
```

### Special Handling
- **Newline characters** (`\n`) are automatically filtered (not displayed)
- Format string **processed character-by-character**
- **Width specifiers** (e.g., `%8x`) are stripped during lexing
- Output appears in **white text on black background** (VGA attribute `0x0F`)

---

## Inline Assembly

Aiden++ supports inline assembly using the `__asm__()` directive for direct x86 instruction embedding.

### Syntax

```c
__asm__("instruction");
```

### Examples

```c
void kernel_main() {
    __asm__("cli");           // Clear interrupts
    __asm__("hlt");           // Halt CPU
    __asm__("mov eax, 0");    // Set EAX to 0
    __asm__("int 0x80");      // Software interrupt
    __asm__("sti");           // Set interrupts
}
```

### Use Cases
- CPU control (interrupts, halt)
- Direct register manipulation
- Port I/O operations
- Performance-critical code sections

---

## Complete Examples

### Example 1: VGA Character Display

```c
struct VGA_Char {
    char c;
    char color;
};

void kernel_main() {
    struct VGA_Char ch = {'A', 0x0F};
    int x = 0xFF;
    print("Char: %c, Val: 0x%x, Shift: %d\n", ch.c, x, x >> 4);
}
```

### Example 2: Pixel Drawing

```c
struct Pixel {
    char x;
    char y;
    char color;
};

void draw_pixel(struct Pixel* p) {
    char* vga = 0xB8000;
    int offset = (p->y * 80 + p->x) * 2;
    *(vga + offset) = '*';
    *(vga + offset + 1) = p->color;
}

void kernel_main() {
    struct Pixel px = {10, 5, 0x0C};
    draw_pixel(&px);
}
```

### Example 3: Bitwise Operations

```c
void kernel_main() {
    int flags = 0xF0;
    int mask = 0x0F;
    
    int result1 = flags | mask;   // 0xFF
    int result2 = flags & mask;   // 0x00
    int result3 = flags >> 4;     // 0x0F
    int result4 = mask << 4;      // 0xF0
    
    print("OR:  0x%x\n", result1);
    print("AND: 0x%x\n", result2);
    print("SHR: 0x%x\n", result3);
    print("SHL: 0x%x\n", result4);
}
```

### Example 4: Loop with Break

```c
void kernel_main() {
    int count = 0;
    
    while (1) {
        print("Count: %d\n", count);
        count++;
        
        if (count >= 10) {
            break;
        }
    }
    
    print("Done!\n");
}
```

---

## Technical Details

### Memory Layout

- **Code Origin:** `0x1000` (4KB offset)
- **VGA Buffer:** `0xB8000` (text mode, 80x25)
- **Architecture:** 32-bit x86 protected mode

### Assembly Output

The compiler generates three sections:

1. **`.text`** - Executable code
2. **`.data`** - Initialized data (string literals)
3. **`.bss`** - Uninitialized data (variables)

### Calling Convention

- **Stack-based** parameter passing (right-to-left)
- **EAX** register for return values
- **Caller-cleanup** for stack arguments

### Variable Naming

All user variables are prefixed with `v_` in assembly output:
- `int x` → `v_x`
- `struct Point p` → `v_p`

---

## Limitations

- No `else` clause for `if` statements
- No `for` loops (use `while` instead)
- No arrays (use pointers instead)
- No string type (use `char*`)
- No floating-point support
- No preprocessor (`#include`, `#define`, etc.)
- No standard library
- Maximum 256 variables per program
- Maximum 256 types (including struct definitions)

---

## Debugging

The compiler outputs extensive debug information to stdout:

- **Tokenization** phase with token types
- **Variable declarations** with types and sizes
- **Expression trees** with operation details
- **Function parsing** progress

Use this information to troubleshoot compilation issues.

---

## License & Credits

**Aiden Compiler** - A specialized C compiler for bare-metal x86 development  
**Language:** Aiden++  
**Author:** PointerToObject  
**Version:** 1.0

---

*Happy bare-metal coding with Aiden++!*
