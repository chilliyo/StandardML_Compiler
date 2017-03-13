// START OF HEADER
	.section .rodata
.output:
	.string "%d\n"
	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
// i := 1;
	pushq	$1
	popq	%rax
	movq	%rax, (i)
// while ((i < 10))
	jmp	lab002
lab001:
// print (i);
	pushq	(i)
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
// i := (i + 1);
	pushq	(i)
	pushq	$1
	popq	%rbx
	popq	%rax
	addq	%rbx, %rax
	pushq	%rax
	popq	%rax
	movq	%rax, (i)
lab002:
	pushq	(i)
	pushq	$10
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	sets	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab001
	popq	%rbp
	ret

	.text
	.globl	factorial
	.type	factorial, @function
factorial:
	pushq	%rbp
	movq	%rsp, %rbp
// (n < 1)
	pushq	16(%rbp)
	pushq	$1
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	sets	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab003
	jmp	lab004
lab003:
// return (1);
	pushq	$1
	popq	%rax
	popq	%rbp
	ret
	jmp	lab005
lab004:
// return ((n * (factorial ((n - 1)))));
	pushq	16(%rbp)
	pushq	16(%rbp)
	pushq	$1
	popq	%rbx
	popq	%rax
	subq	%rbx, %rax
	pushq	%rax
	call	factorial
	addq	$8, %rsp
	pushq	%rax
	popq	%rbx
	popq	%rax
	imulq	%rbx, %rax
	pushq	%rax
	popq	%rax
	popq	%rbp
	ret
lab005:
	popq	%rbp
	ret
	.globl	i
	.data
	.align	8
	.size	i, 8
i:
	.quad	0

	.globl	n
	.data
	.align	8
	.size	n, 8
n:
	.quad	0

