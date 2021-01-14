	.text
	.globl	main
fudas:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq $96, %rax
	pushq %rax
	popq %rax
	jmp fudas_fim
fudas_fim:
	addq $16, %rsp
	popq %rbp
	ret
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $40, %rsp
	call fudas
	addq $0, %rsp
	pushq %rax
	popq %rax
	movq %rax, -16(%rbp)
	movq $42, %rax
	pushq %rax
	popq %rax
	movq %rax, -24(%rbp)
	movq $68, %rax
	pushq %rax
	popq %rax
	movq %rax, -32(%rbp)
	movq $0, %rax
	pushq %rax
	popq %rax
	jmp main_fim
main_fim:
	addq $40, %rsp
	popq %rbp
	ret
printn_int:
	movq %rdi, %rsi
	movq $.Sprintn_int, %rdi
	movq $0, %rax
	call printf
	ret
print_int:
	movq %rdi, %rsi
	movq $.Sprint_int, %rdi
	movq $0, %rax
	call printf
	ret
print_bool:
	cmpq $0, %rdi
	je .print_false
	jne .print_true
.print_true:
	movq %rdi, %rsi
	movq $.true, %rdi
	movq $0, %rax
	call printf
	ret
.print_false:
	movq %rdi, %rsi
	movq $.false, %rdi
	movq $0, %rax
	call printf
	ret
printn_bool:
	cmpq $0, %rdi
	je .printn_false
	jne .printn_true
.printn_true:
	movq %rdi, %rsi
	movq $.truen, %rdi
	movq $0, %rax
	call printf
	ret
.printn_false:
	movq %rdi, %rsi
	movq $.falsen, %rdi
	movq $0, %rax
	call printf
	ret
print_error_z:
	movq %rdi, %rsi
	leaq .Sprint_error_z, %rdi
	movq $0, %rax
	call printf
	jmp main_fim
print_error_f:
	movq %rdi, %rsi
	leaq .Sprint_error_f, %rdi
	movq $0, %rax
	call printf
	jmp main_fim
	.data
.Sprintn_int:
	.string "%ld\n"
.Sprint_int:
	.string "%ld"
.true:
	.string "true"
.false:
	.string "false"
.truen:
	.string "true\n"
.falsen:
	.string "false\n"
.Sprint_error_z:
	.string "\nError: Division by zero.\n\n"
.Sprint_error_f:
	.string "\nFunction without return.\n\n"
is_in_function:
	.quad 0
number_of_loop:
	.quad 0
