	.text
	.align	2
	.globl main
fact:
	addi	$v1, $zero, 8
	sub	$sp, $sp, $v1
	sub	$v1, $fp, $zero
	addi	$fp, $sp, 4
	sw	$ra, 0($fp)
	sw	$v1, -4($fp)
	addi	$sp, $sp, -4
	lw	$v0, 4($fp)
	sw	$v0, 0($sp)
	li	$v0, 0
	lw	$v1, 0($sp)
	addi	$sp, $sp, 4
	sle	$v0, $v1, $v0
	beq	$v0, $zero, L1
	li	$v0, 1
	sub	$a0, $v0, $zero
	j L2
L1:
	addi	$sp, $sp, -4
	lw	$v0, 4($fp)
	sw	$v0, 0($sp)
	addi	$sp, $sp, -4
	lw	$v0, 4($fp)
	sw	$v0, 0($sp)
	li	$v0, 1
	lw	$v1, 0($sp)
	addi	$sp, $sp, 4
	sub	$v0, $v1, $v0
	addi	$sp, $sp, -4
	sw	$v0, 0($sp)
	jal fact
	addi	$sp, $sp, 4
	lw	$v1, 0($sp)
	addi	$sp, $sp, 4
	mul	$v0, $v0, $v1
	sub	$a0, $v0, $zero
L2:
	addi	$sp, $fp, 4
	lw	$ra, 0($fp)
	lw	$fp, -4($fp)
	jr	$ra
g:
	addi	$v1, $zero, 8
	sub	$sp, $sp, $v1
	sub	$v1, $fp, $zero
	addi	$fp, $sp, 4
	sw	$ra, 0($fp)
	sw	$v1, -4($fp)
	addi	$sp, $sp, -4
	lw	$v0, 4($fp)
	addi	$sp, $sp, -4
	sw	$v0, 0($sp)
	jal fact
	addi	$sp, $sp, 4
	sw	$v0, 0($sp)
	lw	$v0, 8($fp)
	addi	$sp, $sp, -4
	sw	$v0, 0($sp)
	jal fact
	addi	$sp, $sp, 4
	lw	$v1, 0($sp)
	addi	$sp, $sp, 4
	add	$v0, $v0, $v1
	sub	$a0, $v0, $zero
	addi	$sp, $fp, 4
	lw	$ra, 0($fp)
	lw	$fp, -4($fp)
	jr	$ra
main:
	addi	$v1, $zero, 16
	sub	$sp, $sp, $v1
	sub	$v1, $fp, $zero
	addi	$fp, $sp, 12
	sw	$ra, 0($fp)
	sw	$v1, -4($fp)
	li	$v0, 3
	sw	$v0, -8($fp)
	addi	$sp, $sp, -4
	lw	$v0, -8($fp)
	sw	$v0, 0($sp)
	li	$v0, 1
	lw	$v1, 0($sp)
	addi	$sp, $sp, 4
	add	$v0, $v0, $v1
	sw	$v0, -12($fp)
	lw	$v0, -12($fp)
	addi	$sp, $sp, -4
	sw	$v0, 0($sp)
	lw	$v0, -8($fp)
	addi	$sp, $sp, -4
	sw	$v0, 0($sp)
	jal g
	addi	$sp, $sp, 8
	addi	$sp, $sp, -4
	sw	$v0, 0($sp)
	jal printInt
	addi	$sp, $sp, 4
	addi	$sp, $fp, 4
	lw	$ra, 0($fp)
	lw	$fp, -4($fp)
	jr	$ra


	.data
	.align 0

#
# below here is the print debugging support code
#
	
.data
_spaceString: .asciiz " "
_newlineString: .asciiz "\n"

.text
.globl printInt     # int reg -> unit
.globl printSpace   # unit    -> unit
.globl printNewline # unit    -> unit

printInt: # int reg->unit
   # The syscall takes its argument in $a0
   lw $a0, 0($29)
   add $t0, $v0, $zero    # since this function does not return anything, it should probably preserve $v0
   li $v0, 1              # print_int syscall
   syscall
   add $v0, $t0, $zero    # restore $v0 
   jr $ra

readInt: # unit->int
   add $t0, $a0, $zero
   li $v0, 5              # read_int
   syscall
   add $a0, $t0, $zero
   jr $ra


printSpace: # unit->unit
   add $t0, $v0, $zero
   la $a0, _spaceString      # address of string to print
   li $v0, 4                 # system call code for print_str
   syscall                   # print the string
   add $v0, $t0, $zero
   jr $ra

printNewline: # unit->unit
   add $t0, $v0, $zero
   la $a0, _newlineString    # address of string to print
   li $v0, 4                 # system call code for print_str
   syscall                   # print the string
   add $v0, $t0, $zero
   jr $ra

