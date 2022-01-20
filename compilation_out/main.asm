global _start
extern printi

section .text

main:
	push rbp
	mov rbp, rsp
	mov rax, 0
	push rax
LOOP_START2:
	mov rax, QWORD [rbp-8]
	push rax
	mov rax, 5
	pop rbx
	cmp rbx, rax
	jl COMPARISON1
	mov rax, 0
	jmp COMPARISON_END1
COMPARISON1:
	mov rax, 1
COMPARISON_END1:
	cmp rax, 0
	je LOOP_END2
	mov rax, QWORD [rbp-8]
	push rax
	pop rdi
	call printi
	mov rax, QWORD [rbp-8]
	add rax, 1
	mov QWORD [rbp-8], rax
	add rsp, 0
	jmp LOOP_START2
LOOP_END2:
	mov rax, 10
	mov QWORD [rbp-8], rax
LOOP_START4:
	mov rax, QWORD [rbp-8]
	push rax
	mov rax, 15
	pop rbx
	cmp rbx, rax
	jl COMPARISON3
	mov rax, 0
	jmp COMPARISON_END3
COMPARISON3:
	mov rax, 1
COMPARISON_END3:
	cmp rax, 0
	je LOOP_END4
	mov rax, QWORD [rbp-8]
	push rax
	pop rdi
	call printi
	mov rax, QWORD [rbp-8]
	add rax, 1
	mov QWORD [rbp-8], rax
	add rsp, 0
	jmp LOOP_START4
LOOP_END4:
	mov rax, 25
	mov QWORD [rbp-8], rax
LOOP_START6:
	mov rax, QWORD [rbp-8]
	push rax
	mov rax, 20
	pop rbx
	cmp rax, rbx
	jle COMPARISON5
	mov rax, 0
	jmp COMPARISON_END5
COMPARISON5:
	mov rax, 1
COMPARISON_END5:
	cmp rax, 0
	je LOOP_END6
	mov rax, QWORD [rbp-8]
	push rax
	pop rdi
	call printi
	mov rax, QWORD [rbp-8]
	sub rax, 1
	mov QWORD [rbp-8], rax
	add rsp, 0
	jmp LOOP_START6
LOOP_END6:
	mov rax, 35
	mov QWORD [rbp-8], rax
LOOP_START8:
	mov rax, QWORD [rbp-8]
	push rax
	mov rax, 30
	pop rbx
	cmp rax, rbx
	jle COMPARISON7
	mov rax, 0
	jmp COMPARISON_END7
COMPARISON7:
	mov rax, 1
COMPARISON_END7:
	cmp rax, 0
	je LOOP_END8
	mov rax, QWORD [rbp-8]
	push rax
	pop rdi
	call printi
	mov rax, QWORD [rbp-8]
	sub rax, 1
	mov QWORD [rbp-8], rax
	add rsp, 0
	jmp LOOP_START8
LOOP_END8:
	mov rax, 40
	mov QWORD [rbp-8], rax
LOOP_START12:
	mov rax, QWORD [rbp-8]
	push rax
	mov rax, 50
	pop rbx
	cmp rbx, rax
	jl COMPARISON9
	mov rax, 0
	jmp COMPARISON_END9
COMPARISON9:
	mov rax, 1
COMPARISON_END9:
	cmp rax, 0
	je LOOP_END12
	mov rax, QWORD [rbp-8]
	push rax
	mov rax, 45
	pop rbx
	cmp rax, rbx
	je COMPARISON10
	mov rax, 0
	jmp COMPARISON_END10
COMPARISON10:
	mov rax, 1
COMPARISON_END10:
	cmp rax, 0
	je ELSE11
	mov rax, QWORD [rbp-8]
	push rax
	pop rdi
	call printi
	add rsp, 0
	jmp END11
ELSE11:
END11:
	mov rax, QWORD [rbp-8]
	add rax, 1
	mov QWORD [rbp-8], rax
	add rsp, 0
	jmp LOOP_START12
LOOP_END12:
	add rsp, 8
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
