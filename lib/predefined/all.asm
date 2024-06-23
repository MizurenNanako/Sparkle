newframe:
    addi    $sp,    $sp,    -8  # allocate frame
    sw      $ra,    4($sp)      # return addr
    sw      $fp,    0($sp)      # old frame

delframe:
    lw      $fp,    0($sp)      # old frame
    lw      $ra,    4($sp)      # return addr
    addi    $sp,    $sp,    8   # dealloc frame

push1:
    addi    $sp,    $sp,    -4
    sw      $s0,    $sp

pop1:
    lw      $s0,    0($sp)
    addi    $sp,    $sp,    4

add2:
    lw      $t1,    0($sp)      # Load 4 bytes from $sp into $t1
    addi    $sp,    $sp,    4   # Add 4 to $sp
    lw      $t2,    0($sp)      # Load 4 bytes from $sp into $t2
    add     $t1,    $t1,    $t2 # Add $t1 and $t2, store result in $t1
    sw      $t1,    0($sp)      # Store the value of $t1 back to $sp

addu2:
    lw      $t1,    0($sp)      # Load 4 bytes from $sp into $t1
    addi    $sp,    $sp,    4   # Add 4 to $sp
    lw      $t2,    0($sp)      # Load 4 bytes from $sp into $t2
    addu    $t1,    $t1,    $t2 # Add $t1 and $t2 using unsigned addition, store result in $t1
    sw      $t1,    0($sp)      # Store the value of $t1 back to $sp

sub2:
    lw      $t1,    0($sp)      # Load 4 bytes from $sp into $t1
    addi    $sp,    $sp,    4   # Add 4 to $sp
    lw      $t2,    0($sp)      # Load 4 bytes from $sp into $t2
    sub     $t1,    $t1,    $t2 # Subtract $t2 from $t1, store result in $t1
    sw      $t1,    0($sp)      # Store the value of $t1 back to $sp

subu2:
    lw      $t1,    0($sp)      # Load 4 bytes from $sp into $t1
    addi    $sp,    $sp,    4   # Add 4 to $sp
    lw      $t2,    0($sp)      # Load 4 bytes from $sp into $t2
    subu    $t1,    $t1,    $t2 # Subtract $t2 from $t1 using unsigned subtraction, store result in $t1
    sw      $t1,    0($sp)      # Store the value of $t1 back to $sp

mul2:
    lw      $t1,    0($sp)      # Load 4 bytes from $sp into $t1
    addi    $sp,    $sp,    4   # Add 4 to $sp
    lw      $t2,    0($sp)      # Load 4 bytes from $sp into $t2
    mul     $t1,    $t1,    $t2 # Multiply $t1 by $t2, store the lower 32 bits of the result in $t1
    sw      $t1,    0($sp)      # Store the value of $t1 back to $sp

mulu2:
    lw      $t1,    0($sp)      # Load 4 bytes from $sp into $t1
    addi    $sp,    $sp,    4   # Add 4 to $sp
    lw      $t2,    0($sp)      # Load 4 bytes from $sp into $t2
    mulu    $t1,    $t1,    $t2 # Multiply $t1 by $t2 using unsigned multiplication, store the lower 32 bits of the result in $t1
    sw      $t1,    0($sp)      # Store the value of $t1 back to $sp

div2:
    lw      $t1,    0($sp)      # Load 4 bytes from $sp into $t1
    addi    $sp,    $sp,    4   # Add 4 to $sp
    lw      $t2,    0($sp)      # Load 4 bytes from $sp into $t2
    div     $t1,    $t1,    $t2 # Divide $t1 by $t2, store the quotient in $t1 (assumes $t2 is not zero)
    sw      $t1,    0($sp)      # Store the value of $t1 back to $sp

divu2:
    lw      $t1,    0($sp)      # Load 4 bytes from $sp into $t1
    addi    $sp,    $sp,    4   # Add 4 to $sp
    lw      $t2,    0($sp)      # Load 4 bytes from $sp into $t2
    divu    $t1,    $t1,    $t2 # Divide $t1 by $t2 using unsigned division, store the quotient in $t1 (assumes $t2 is not zero)
    mflo    $t1                 # Move the quotient from LO to $t1
    sw      $t1,    0($sp)      # Store the value of $t1 back to $sp

mod2:
    lw      $t1,    0($sp)      # Load 4 bytes from $sp into $t1
    addi    $sp,    $sp,    4   # Add 4 to $sp
    lw      $t2,    0($sp)      # Load 4 bytes from $sp into $t2
    div     $t1,    $t1,    $t2 # Divide $t1 by $t2
    mfhi    $t1                 # Move the remainder from HI to $t1
    sw      $t1,    0($sp)      # Store the value of $t1 back to $sp


modu2:
    lw      $t1,    0($sp)      # Load 4 bytes from $sp into $t1
    addi    $sp,    $sp,    4   # Add 4 to $sp
    lw      $t2,    0($sp)      # Load 4 bytes from $sp into $t2
    divu    $t1,    $t1,    $t2 # Divide $t1 by $t2 using unsigned division
    mfhi    $t1                 # Move the remainder from HI to $t1
    sw      $t1,    0($sp)      # Store the value of $t1 back to $sp

not1:
    lw      $t1,    0($sp)      # Load 4 bytes from $sp into $t1
    not     $t1,    $t1         # Perform bitwise NOT on $t1
    sw      $t1,    0($sp)      # Store the value of $t1 back to $sp

xor2:
    lw      $t1,    0($sp)      # sp -> t1
    addi    $sp,    $sp,    4   # sp += 4
    lw      $t2,    0($sp)      # sp -> t2
    xor     $t1,    $t1,    $t2 # t1 = t1 xor t2
    sw      $t1,    0($sp)      # sp <- t1

and2:
    lw      $t1,    0($sp)      # sp -> t1
    addi    $sp,    $sp,    4   # sp += 4
    lw      $t2,    0($sp)      # sp -> t2
    and     $t1,    $t1,    $t2 # t1 = t1 and t2
    sw      $t1,    0($sp)      # sp <- t1

or2:
    lw      $t1,    0($sp)      # sp -> t1
    addi    $sp,    $sp,    4   # sp += 4
    lw      $t2,    0($sp)      # sp -> t2
    or      $t1,    $t1,    $t2 # t1 = t1 or t2
    sw      $t1,    0($sp)      # sp <- t1

shl2:
    lw      $t1,    0($sp)      # sp -> t1
    addi    $sp,    $sp,    4   # sp += 4
    lw      $t2,    0($sp)      # sp -> t2
    sllv    $t1,    $t1,    $t2 # t1 = t1 << t2 (variable shift left)
    sw      $t1,    0($sp)      # sp <- t1

shr:
    lw      $t1,    0($sp)      # sp -> t1
    addi    $sp,    $sp,    4   # sp += 4
    lw      $t2,    0($sp)      # sp -> t2
    srav    $t1,    $t1,    $t2 # t1 = t1 >> t2 (arithmetic right shift)
    sw      $t1,    0($sp)      # sp <- t1

lshr:
    lw      $t1,    0($sp)      # sp -> t1
    addi    $sp,    $sp,    4   # sp += 4
    lw      $t2,    0($sp)      # sp -> t2
    srlv    $t1,    $t1,    $t2 # t1 = t1 >> t2 (logical right shift)
    sw      $t1,    0($sp)      # sp <- t1

