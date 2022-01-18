global _start
extern printi

section .text

main:
	push rbp
	mov rbp, rsp
	mov rax, 0
	mov QWORD [rbp-8], rax
	sub rsp, 8
LOOP_START1:
	mov rax, QWORD [rbp-8]
	sub rax, 20
	cmp rax, 0
	je LOOP_END1
	mov rax, QWORD [rbp-8]
	push rax
	pop rdi
	call printi
	mov rax, QWORD [rbp-8]
	add rax, 1
	mov QWORD [rbp-8], rax
	add rsp, 0
	jmp LOOP_START1
LOOP_END1:
	add rsp, 8
	pop rbp
	ret
_start:
	push rbp
	mov rbp, rsp

	call main

	pop rbp

	mov rax, 60
	mov rdi, 0
	syscall
