	.text
	.align	2
	.globl main
main:
	addi $sp, $sp, -4
	sw $ra, 0($sp)
	addi $sp, $sp, -4
	sw $fp, 0($sp)
	addi $fp, $sp, 0
	nop
	addi $sp, $sp, -4
	li $t1, 12
	addi $sp, $sp, -4
	sw $t1, 0($sp)
	j f
	addi $sp, $sp, 4
	addi $sp, $fp, 0
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	jr $ra

f:
	addi $sp, $sp, -4
	sw $ra, 0($sp)
	addi $sp, $sp, -4
	sw $fp, 0($sp)
	addi $fp, $sp, 0
	nop
	addi $sp, $sp, -4
	lw $t1, 8($fp)
	j printi
	addi $sp, $sp, 4
	addi $sp, $fp, 0
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	jr $ra

	.data


	.text
printi:
    addi    $sp,    $sp,    -4  #frame
    sw      $ra,    0($sp)
    addi    $sp,    $sp,    -4
    sw      $fp,    0($sp)
    addi    $fp,    $sp,    0

    lw      $a0,    8($fp)      # arg0
    li      $v0,    1           # print_int syscall
    syscall 
    jr      $ra
