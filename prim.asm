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