	.text
	.file	"scic"
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3               # -- Begin function main
.LCPI0_0:
	.quad	4616275186447678439     # double 4.0759999999999996
.LCPI0_1:
	.quad	-4613145184706655945    # double -1.6759999999999999
.LCPI0_2:
	.quad	4614953830317007935     # double 3.4511999999999996
.LCPI0_3:
	.quad	4602678819172646912     # double 0.5
.LCPI0_4:
	.quad	4601193289441697980     # double 0.41753653444676408
.LCPI0_5:
	.quad	4611686018427387904     # double 2
.LCPI0_6:
	.quad	4613937818241073152     # double 3
.LCPI0_7:
	.quad	4609434218613702656     # double 1.5
.LCPI0_8:
	.quad	4621256167635550208     # double 9
.LCPI0_9:
	.quad	4640057816470519808     # double 166.375
	.text
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%r14
	.cfi_def_cfa_offset 16
	pushq	%rbx
	.cfi_def_cfa_offset 24
	pushq	%rax
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -24
	.cfi_offset %r14, -16
	leaq	.Lfmt.1(%rip), %r14
	movsd	.LCPI0_0(%rip), %xmm0   # xmm0 = mem[0],zero
	movq	%r14, %rdi
	movb	$1, %al
	callq	printf@PLT
	movsd	.LCPI0_1(%rip), %xmm0   # xmm0 = mem[0],zero
	movq	%r14, %rdi
	movb	$1, %al
	callq	printf@PLT
	movsd	.LCPI0_2(%rip), %xmm0   # xmm0 = mem[0],zero
	movq	%r14, %rdi
	movb	$1, %al
	callq	printf@PLT
	movsd	.LCPI0_3(%rip), %xmm0   # xmm0 = mem[0],zero
	movq	%r14, %rdi
	movb	$1, %al
	callq	printf@PLT
	movsd	.LCPI0_4(%rip), %xmm0   # xmm0 = mem[0],zero
	movq	%r14, %rdi
	movb	$1, %al
	callq	printf@PLT
	leaq	.Lfmt(%rip), %rbx
	movq	%rbx, %rdi
	movl	$1, %esi
	xorl	%eax, %eax
	callq	printf@PLT
	movq	%rbx, %rdi
	movl	$1, %esi
	xorl	%eax, %eax
	callq	printf@PLT
	movq	%rbx, %rdi
	movl	$1, %esi
	xorl	%eax, %eax
	callq	printf@PLT
	movq	%rbx, %rdi
	xorl	%esi, %esi
	xorl	%eax, %eax
	callq	printf@PLT
	movsd	.LCPI0_5(%rip), %xmm0   # xmm0 = mem[0],zero
	movsd	.LCPI0_6(%rip), %xmm1   # xmm1 = mem[0],zero
	callq	pow@PLT
	movq	%r14, %rdi
	movb	$1, %al
	callq	printf@PLT
	movsd	.LCPI0_7(%rip), %xmm1   # xmm1 = mem[0],zero
	movsd	.LCPI0_5(%rip), %xmm0   # xmm0 = mem[0],zero
	callq	pow@PLT
	movq	%r14, %rdi
	movb	$1, %al
	callq	printf@PLT
	movsd	.LCPI0_8(%rip), %xmm0   # xmm0 = mem[0],zero
	movq	%r14, %rdi
	movb	$1, %al
	callq	printf@PLT
	movsd	.LCPI0_9(%rip), %xmm0   # xmm0 = mem[0],zero
	movq	%r14, %rdi
	movb	$1, %al
	callq	printf@PLT
	xorl	%eax, %eax
	addq	$8, %rsp
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%r14
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	.Lfmt,@object           # @fmt
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmt:
	.asciz	"%d\n"
	.size	.Lfmt, 4

	.type	.Lfmt.1,@object         # @fmt.1
.Lfmt.1:
	.asciz	"%g\n"
	.size	.Lfmt.1, 4

	.type	.Lfmt.2,@object         # @fmt.2
.Lfmt.2:
	.asciz	"%s\n"
	.size	.Lfmt.2, 4

	.section	".note.GNU-stack","",@progbits
