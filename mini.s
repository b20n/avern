	.section	__TEXT,__text,regular,pure_instructions
	.globl	_go
	.align	4, 0x90
_go:                                    ## @go
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp2:
	.cfi_def_cfa_offset 16
Ltmp3:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp4:
	.cfi_def_cfa_register %rbp
	movq	_foo@GOTPCREL(%rip), %rax
	movl	(%rax), %ecx
	addl	$1, %ecx
	movl	%ecx, (%rax)
	popq	%rbp
	retq
	.cfi_endproc


.subsections_via_symbols
