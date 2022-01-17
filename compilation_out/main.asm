global _start
extern printi

section .text

face:
	push rbp
	mov rbp, rsp
	mov rax, 1
	add rax, 5
	push rax
	pop rdi
	call printi
	add rsp, 0
	pop rbp
	ret
main:
	push rbp
	mov rbp, rsp
	mov rax, 1
	add rax, 3
	push rax
	pop rdi
	call printi
	mov rax, 1
	mov QWORD [rbp-8], rax
	sub rsp, 8
	call face
	mov rax, QWORD [rbp-8]
	add rax, 4
	push rax
	pop rdi
	call printi
	mov rax, QWORD [rbp-8]
	add rax, 2
	mov QWORD [rbp-8], rax
	mov rax, QWORD [rbp-8]
	add rax, 4
	push rax
	pop rdi
	call printi
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
