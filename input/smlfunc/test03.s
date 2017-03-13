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
	jmp	lab009
lab008:
// print ((mod (i)));
	pushq	(i)
	call	mod
	addq	$8, %rsp
	pushq	%rax
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
lab009:
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
	jne	lab008
	popq	%rbp
	ret

	.text
	.globl	mod
	.type	mod, @function
mod:
	pushq	%rbp
	movq	%rsp, %rbp
// ((n % 3) = 0)
	pushq	16(%rbp)
	pushq	$3
	popq	%rbx
	popq	%rax
	movq	$0, %rdx
	idivq	%rbx
	pushq	%rax
	pushq	$0
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	sete	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab010
	jmp	lab011
lab010:
// return (n);
	pushq	16(%rbp)
	popq	%rax
	popq	%rbp
	ret
	jmp	lab012
lab011:
// return (0);
	pushq	$0
	popq	%rax
	popq	%rbp
	ret
lab012:
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

