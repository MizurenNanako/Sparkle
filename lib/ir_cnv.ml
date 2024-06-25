module Stack_ir_cnv = struct
  module E = Typing.Env
  module A = Syntactics.AST2
  module S = Stack_ir.StkIR
end

module Llvm_ir_cnv = struct
  module E = Typing.Env
  module A = Syntactics.AST2

  let compile_constant_table () = E.dirty_table.dirty_constant_table
end
