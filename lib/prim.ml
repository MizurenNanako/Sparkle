let print =
  "#\n\
   # below here is the print debugging support code\n\
   #\n\
   \t\n\
   .data\n\
   _spaceString: .asciiz \" \"\n\
   _newlineString: .asciiz \"\\n\"\n\n\
   .text\n\
   .globl printInt     # int reg -> unit\n\
   .globl printSpace   # unit    -> unit\n\
   .globl printNewline # unit    -> unit\n\n\
   printInt: # int reg->unit\n\
  \   # The syscall takes its argument in $a0\n\
  \   lw $a0, 0($29)\n\
  \   add $t0, $v0, $zero    # since this function does not return anything, it \
   should probably preserve $v0\n\
  \   li $v0, 1              # print_int syscall\n\
  \   syscall\n\
  \   add $v0, $t0, $zero    # restore $v0 \n\
  \   jr $ra\n\n\
   readInt: # unit->int\n\
  \   add $t0, $a0, $zero\n\
  \   li $v0, 5              # read_int\n\
  \   syscall\n\
  \   add $a0, $t0, $zero\n\
  \   jr $ra\n\n\n\
   printSpace: # unit->unit\n\
  \   add $t0, $v0, $zero\n\
  \   la $a0, _spaceString      # address of string to print\n\
  \   li $v0, 4                 # system call code for print_str\n\
  \   syscall                   # print the string\n\
  \   add $v0, $t0, $zero\n\
  \   jr $ra\n\n\
   printNewline: # unit->unit\n\
  \   add $t0, $v0, $zero\n\
  \   la $a0, _newlineString    # address of string to print\n\
  \   li $v0, 4                 # system call code for print_str\n\
  \   syscall                   # print the string\n\
  \   add $v0, $t0, $zero\n\
  \   jr $ra\n"
;;
