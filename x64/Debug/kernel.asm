[BITS 32]
global kernel_main
section .text

start:
    call kernel_main
    jmp $ ; hang

kernel_main:
    mov byte [0xB8000], 15
    mov byte [0xB8001], 0x0F
    mov eax, 69420
    ret
