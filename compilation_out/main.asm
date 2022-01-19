global _start
extern printi

section .text

testtwo:
	push rbp
	mov rbp, rsp
	push rdi
	push rsi
	push rcx
	mov rax, QWORD [rbp-8]
	push rax
	mov rax, QWORD [rbp-16]
	pop rbx
	add rax, rbx
	push rax
	mov rax, QWORD [rbp-24]
	pop rbx
	add rax, rbx
	add rax, 5
	add rsp, 0
	add rsp, 24
	pop rbp
	ret
test:
	push rbp
	mov rbp, rsp
	push rdi
	push rsi
	mov rax, QWORD [rbp-8]
	push rax
	mov rax, QWORD [rbp-16]
	pop rbx
	add rax, rbx
	push rax
	mov rax, QWORD [rbp-8]
	push rax
	mov rax, QWORD [rbp-16]
	push rax
	mov rax, 3
	push rax
	pop rcx
	pop rsi
	pop rdi
	call testtwo
	pop rbx
	add rax, rbx
	add rsp, 0
	add rsp, 16
	pop rbp
	ret
main:
	push rbp
	mov rbp, rsp
	mov rax, 1
	push rax
	mov rax, 2
	push rax
	pop rsi
	pop rdi
	call test
	push rax
	pop rdi
	call printi
	mov rax, 3
	push rax
	mov rax, 4
	push rax
	pop rsi
	pop rdi
	call test
	push rax
	pop rdi
	call printi
	add rsp, 0
	add rsp, 0
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
