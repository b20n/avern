	.section	__TEXT,__text,regular,pure_instructions
	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
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
	movl	$0, %eax
	movl	$0, -4(%rbp)
	movl	_foo(%rip), %ecx
	addl	$1, %ecx
	movl	%ecx, _foo(%rip)
	popq	%rbp
	retq
	.cfi_endproc

	.section	__DATA,__data
	.globl	_foo                    ## @foo
	.align	2
_foo:
	.long	4                       ## 0x4


.subsections_via_symbols
