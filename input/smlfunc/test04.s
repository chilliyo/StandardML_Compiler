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
// print_literal_string (Hello World);
	.section	.rodata
lab013:
	.string	"Hello World"
	.text
	movl	$lab013, %edi
	call	puts
// print_literal_string (Printing From a second print_literal_string());
	.section	.rodata
lab014:
	.string	"Printing From a second print_literal_string()"
	.text
	movl	$lab014, %edi
	call	puts
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
