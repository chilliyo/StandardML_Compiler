 	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
// a := (newarray (10));
	pushq	$10
	popq	%rdi
	imulq	$8, %rdi
	call	malloc
	pushq	%rax
	popq	%rax
	movq	%rax, (a)
// for (i := 0 to 25)
	pushq	$0
	popq	%rax
	movq	%rax, (i)
	jmp	lab014
lab013:
// dummy := (write (a, 0, (i + 65)));
	pushq	(i)
	pushq	$65
	popq	%rbx
	popq	%rax
	addq	%rbx, %rax
	pushq	%rax
	pushq	$0
	pushq	(a)
	call	write
	addq	$24, %rsp
	pushq	%rax
	popq	%rax
	movq	%rax, (dummy)
// dummy := (write (a, 1, 10));
	pushq	$10
	pushq	$1
	pushq	(a)
	call	write
	addq	$24, %rsp
	pushq	%rax
	popq	%rax
	movq	%rax, (dummy)
// printstring (a);
	pushq	(a)
	popq	%rdi
	call	print_string
// i := (i + 1);
	pushq	(i)
	pushq	$1
	popq	%rbx
	popq	%rax
	addq	%rbx, %rax
	pushq	%rax
	popq	%rax
	movq	%rax, (i)
	movq	(i), %rax
	addq	$1, %rax
	movq	%rax, (i)
lab014:
	pushq	$25
	popq	%rbx
	movq	(i), %rax
	cmpq	%rbx, %rax
	jle	lab013
	popq	%rbp
	ret

	.text
	.globl	hello
	.type	hello, @function
hello:
	pushq	%rbp
	movq	%rsp, %rbp
	popq	%rbp
	ret

	.text
	.globl	print_string
	.type	print_string, @function
print_string:
.LFB0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	jmp	.L2
.L3:
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	movsbl	%al, %eax
	movl	%eax, %edi
	call	putchar
	addq	$8, -8(%rbp)
.L2:
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.L3
	leave
	ret

	.section .rodata
.output:
	.string "%d\n"

	.globl	a
	.data
	.align	8
	.size	a, 8
a:
	.quad	0

	.globl	dummy
	.data
	.align	8
	.size	dummy, 8
dummy:
	.quad	0

	.globl	i
	.data
	.align	8
	.size	i, 8
i:
	.quad	0

