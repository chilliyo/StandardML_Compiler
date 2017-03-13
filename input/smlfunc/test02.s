// START OF HEADER
	.section .rodata
.output:
	.string "%d\n"
// END OF HEADER
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
// while ((i < 101))
	jmp	lab007
lab006:
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
lab007:
	pushq	(i)
	pushq	$101
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	sets	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab006
	popq	%rbp
	ret

	.text
	.globl	numbers
	.type	numbers, @function
numbers:
	pushq	%rbp
	movq	%rsp, %rbp
	popq	%rbp
	ret
	.globl	i
	.data
	.align	8
	.size	i, 8
i:
	.quad	0

