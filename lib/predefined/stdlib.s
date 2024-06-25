	.text
	.abicalls
	.option	pic0
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.text
	.file	"stdlib.ll"
	.globl	main                            # -- Begin function main
	.p2align	2
	.type	main,@function
	.set	nomicromips
	.set	nomips16
	.ent	main
main:                                   # @main
	.frame	$sp,24,$ra
	.mask 	0x80000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# %bb.0:                                # %entry
	addiu	$sp, $sp, -24
	sw	$ra, 20($sp)                    # 4-byte Folded Spill
	lui	$1, %hi($.str)
	jal	printf
	addiu	$4, $1, %lo($.str)
	addiu	$2, $zero, 0
	lw	$ra, 20($sp)                    # 4-byte Folded Reload
	jr	$ra
	addiu	$sp, $sp, 24
	.set	at
	.set	macro
	.set	reorder
	.end	main
$func_end0:
	.size	main, ($func_end0)-main
                                        # -- End function
	.type	$.str,@object                   # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
$.str:
	.asciz	"Hello, World!"
	.size	$.str, 14

	.section	".note.GNU-stack","",@progbits
	.text
