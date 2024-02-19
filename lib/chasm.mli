open Chasm_types
open Stdint

val al:   [> `r8 of r8]
val cl:   [> `r8 of r8]
val dl:   [> `r8 of r8]
val bl:   [> `r8 of r8]
val sil:  [> `r8 of r8]
val dil:  [> `r8 of r8]
val spl:  [> `r8 of r8]
val bpl:  [> `r8 of r8]
val r8b:  [> `r8 of r8]
val r9b:  [> `r8 of r8]
val r10b: [> `r8 of r8]
val r11b: [> `r8 of r8]
val r12b: [> `r8 of r8]
val r13b: [> `r8 of r8]
val r14b: [> `r8 of r8]
val r15b: [> `r8 of r8]

val ax:   [> `r16 of r16]
val cx:   [> `r16 of r16]
val dx:   [> `r16 of r16]
val bx:   [> `r16 of r16]
val si:   [> `r16 of r16]
val di:   [> `r16 of r16]
val sp:   [> `r16 of r16]
val bp:   [> `r16 of r16]
val r8w:  [> `r16 of r16]
val r9w:  [> `r16 of r16]
val r10w: [> `r16 of r16]
val r11w: [> `r16 of r16]
val r12w: [> `r16 of r16]
val r13w: [> `r16 of r16]
val r14w: [> `r16 of r16]
val r15w: [> `r16 of r16]

val eax:  [> `r32 of r32]
val ecx:  [> `r32 of r32]
val edx:  [> `r32 of r32]
val ebx:  [> `r32 of r32]
val esi:  [> `r32 of r32]
val edi:  [> `r32 of r32]
val esp:  [> `r32 of r32]
val ebp:  [> `r32 of r32]
val r8d:  [> `r32 of r32]
val r9d:  [> `r32 of r32]
val r10d: [> `r32 of r32]
val r11d: [> `r32 of r32]
val r12d: [> `r32 of r32]
val r13d: [> `r32 of r32]
val r14d: [> `r32 of r32]
val r15d: [> `r32 of r32]

val rax: [> `r64 of r64]
val rcx: [> `r64 of r64]
val rdx: [> `r64 of r64]
val rbx: [> `r64 of r64]
val rsi: [> `r64 of r64]
val rdi: [> `r64 of r64]
val rsp: [> `r64 of r64]
val rbp: [> `r64 of r64]
val r8:  [> `r64 of r64]
val r9:  [> `r64 of r64]
val r10: [> `r64 of r64]
val r11: [> `r64 of r64]
val r12: [> `r64 of r64]
val r13: [> `r64 of r64]
val r14: [> `r64 of r64]
val r15: [> `r64 of r64]

val rb: int -> [> `r8 of r8]
val rw: int -> [> `r16 of r16]
val rd: int -> [> `r32 of r32]
val rq: int -> [> `r64 of r64]

val imm:   int   -> [> `imm of int]
val imm8:  int8  -> [> `imm8 of int8]
val imm8_i: int  -> [> `imm8 of int8]
val imm16: int16 -> [> `imm16 of int16]
val imm16_i: int -> [> `imm16 of int16]
val imm32: int32 -> [> `imm32 of int32]
val imm32_i: int -> [> `imm32 of int32]
val imm64: int64 -> [> `imm64 of int64]

class mem_op_base_plus_reg :
  r64 * r64 ->
object
  method build : [> `mem of mem ]
end

class mem_op_base : r64 -> object
  method build : [> `mem of mem ]

  method plus_reg :
    [ `r64 of r64 ] -> mem_op_base_plus_reg
end

val (++) : mem_op_base -> [ `r64 of r64] -> mem_op_base_plus_reg

class mem_op : object
    method base : r64 -> mem_op_base
end

val qword_ptr: [< `r64 of r64 ] -> mem_op_base

val push: push_type -> instruction

exception Invalid_encoding of string

val assemble: instruction -> bytes
val assemble_list: instruction list -> bytes