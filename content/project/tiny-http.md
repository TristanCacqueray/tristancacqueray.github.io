---
title: tiny-http
date: 2014-07-20
tags: [cli]
---

A tiny http server in 145 byte.

Demo:

```c
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <stdio.h>

unsigned char shellcode[] =
"\x48\x89\xe5\x31\xd2\x31\xf6\xff\xc6\x31\xff\x40\xb7\x02\x31\xc0\xb0\x29"
"\x0f\x05\x89\xc5\x48\x31\xc0\x50\x50\x68\x02\x01\x04\xd2\x48\x89\x44\x24"
"\x04\xb0\x02\x66\x89\x04\x24\x48\x89\xe6\x31\xd2\xb2\x10\x89\xef\xb0\x31"
"\x0f\x05\x48\x31\xc0\x48\x31\xff\x48\x31\xf6\xb0\x32\x89\xef\x40\xb6\x09"
"\x0f\x05\x31\xc0\xb0\x2b\x89\xef\x31\xf6\x31\xd2\x0f\x05\x49\xb8\x74\x6f"
"\x75\x63\x68\x20\x2d\x69\x41\x50\x49\xb8\x20\x32\x30\x30\x0d\x0a\x0d\x0a"
"\x41\x50\x49\xb8\x48\x54\x54\x50\x2f\x31\x2e\x30\x41\x50\x89\xc7\x31\xc0"
"\xb0\x01\x48\x89\xe6\xb2\x18\x0f\x05\x48\x31\xc0\xb0\x3c\x48\x31\xff\x0f"
"\x05";

int main ()
{
    printf("Length: %d bytes\n", strlen(shellcode));
    void *buf;

    buf = mmap(NULL, strlen(shellcode) + 1, PROT_READ|PROT_WRITE|PROT_EXEC, MAP_ANONYMOUS|MAP_SHARED, -1, 0);
    if (buf == MAP_FAILED){
        printf("mmap failed");
        return 0;
    }
    strcpy(buf, shellcode);
    printf("Go to http://localhost:1234\n");
    int (*sc)() = (int (*)())buf;
    sc();
    return 0;
}
```


The assembly version:

```asm
        .globl main
        .text

main:
        mov    %rsp,%rbp

socket:
        xor    %edx, %edx
        xor    %esi, %esi
        inc     %esi
        xor    %edi, %edi
        movb    $0x2,%dil
        xor     %eax, %eax
        movb $0x29,%al
        syscall
        mov %eax, %ebp

sockaddr:
        xor %rax, %rax
        push %rax
        push %rax
        push $0xffffffffd2040102
        mov %rax,4(%rsp)
        movb $0x2, %al
        mov %ax, (%rsp)
        mov %rsp, %rsi

bind:
        xor %edx, %edx
        movb    $0x10,%dl
        mov %ebp, %edi
        movb $0x31,%al
        syscall

listen:
        xor %rax, %rax
        xor %rdi, %rdi
        xor %rsi, %rsi
        movb $0x32, %al
        mov %ebp, %edi
        movb $0x9, %sil
        syscall

accept:
        xor %eax, %eax
        movb $0x2b, %al
        mov %ebp, %edi
        xor %esi, %esi
        xor %edx, %edx
        syscall

document:
        // "HTTP/1.0 200\r\n\r\ntouch -i"
        mov $0x692d206863756f74, %r8
        push %r8
        mov $0x0a0d0a0d30303220, %r8
        push %r8
        mov $0x302e312f50545448, %r8
        push %r8

write:
        mov %eax, %edi      // client_fd
        xor %eax, %eax
        movb $0x1, %al
        mov %rsp, %rsi
        movb $24, %dl
        syscall

over:
        xor %rax, %rax
        movb $60, %al
        xor %rdi, %rdi
        syscall
```
