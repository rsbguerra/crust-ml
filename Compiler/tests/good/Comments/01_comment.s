	.text
	.globl	main
addOne:
	pushq %rbp
	movq %rsp, %rbp
	subq $24, %rsp
	movq 16(%rbp), %rax
	pushq %rax
	movq $10, %rax
	pushq %rax
	popq %rbx
	popq %rax
	cmpq %rbx, %rax
	jg bool_true_1
	movq $0, %rax
	pushq %rax
	jmp bool_end_1
bool_true_1:
	movq $1, %rax
	pushq %rax
bool_end_1:
	popq %rax
	cmpq $0, %rax
	je if_else_11
	movq 16(%rbp), %rax
	pushq %rax
	popq %rax
	jmp addOne_fim
	jmp if_end_1
if_else_11:
if_end_1:
	movq 16(%rbp), %rax
	pushq %rax
	popq %rdi
	call printn_int
	movq 16(%rbp), %rax
	pushq %rax
	movq $1, %rax
	pushq %rax
	popq %rax
	popq %rbx
	addq %rax, %rbx
	pushq %rbx
	call addOne
	addq $8, %rsp
	pushq %rax
	popq %rax
	jmp addOne_fim
addOne_fim:
	addq $24, %rsp
	popq %rbp
	ret
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq $5, %rax
	pushq %rax
	popq %rdi
	call printn_int
	movq $0, %rax
	pushq %rax
	popq %rax
	jmp main_fim
main_fim:
	addq $16, %rsp
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
