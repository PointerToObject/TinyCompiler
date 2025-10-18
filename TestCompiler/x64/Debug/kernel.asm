[BITS 32]
global kernel_main
section .text

start:
    call kernel_main
    jmp $ ; hang

kernel_main:
    mov eax, 69
    ret
