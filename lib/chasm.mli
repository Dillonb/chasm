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

(** [reg] *)
val word_ptr_of_r64: [`r64 of r64 ] -> [> `mem16 of mem ]

(** [reg + reg] *)
val word_ptr_of_r64_plus_r64: [ `r64 of r64 ] -> [ `r64 of r64 ] -> [> `mem16 of mem ]

(** [reg + reg * scale] *)
val word_ptr_of_r64_plus_r64_scaled: [ `r64 of r64 ] -> [ `r64 of r64 ] -> int -> [> `mem16 of mem ]

(** [reg * scale] *)
val word_ptr_of_r64_scaled: [ `r64 of r64 ] -> int -> [> `mem16 of mem ]

(** [reg + offset] *)
val word_ptr_of_r64_plus_offset: [`r64 of r64 ] -> int -> [> `mem16 of mem ]

(** [reg + reg + offset] *)
val word_ptr_of_r64_plus_r64_plus_offset: [ `r64 of r64 ] -> [ `r64 of r64 ] -> int -> [> `mem16 of mem ]

(** [reg + reg * scale + offset] *)
val word_ptr_of_r64_plus_r64_scaled_plus_offset: [ `r64 of r64 ] -> [ `r64 of r64 ] -> int -> int -> [> `mem16 of mem ]

(** [reg * scale + offset] *)
val word_ptr_of_r64_scaled_plus_offset: [ `r64 of r64 ] -> int -> int -> [> `mem16 of mem ]

(** [reg] *)
val qword_ptr_of_r64: [`r64 of r64 ] -> [> `mem64 of mem ]

(** [reg + reg] *)
val qword_ptr_of_r64_plus_r64: [ `r64 of r64 ] -> [ `r64 of r64 ] -> [> `mem64 of mem ]

(** [reg + reg * scale] *)
val qword_ptr_of_r64_plus_r64_scaled: [ `r64 of r64 ] -> [ `r64 of r64 ] -> int -> [> `mem64 of mem ]

(** [reg * scale] *)
val qword_ptr_of_r64_scaled: [ `r64 of r64 ] -> int -> [> `mem64 of mem ]

(** [reg + offset] *)
val qword_ptr_of_r64_plus_offset: [`r64 of r64 ] -> int -> [> `mem64 of mem ]

(** [reg + reg + offset] *)
val qword_ptr_of_r64_plus_r64_plus_offset: [ `r64 of r64 ] -> [ `r64 of r64 ] -> int -> [> `mem64 of mem ]

(** [reg + reg * scale + offset] *)
val qword_ptr_of_r64_plus_r64_scaled_plus_offset: [ `r64 of r64 ] -> [ `r64 of r64 ] -> int -> int -> [> `mem64 of mem ]

(** [reg * scale + offset] *)
val qword_ptr_of_r64_scaled_plus_offset: [ `r64 of r64 ] -> int -> int -> [> `mem64 of mem ]

(** [reg] *)
val word_ptr_of_r32: [`r32 of r32 ] -> [> `mem16 of mem ]

(** [reg + reg] *)
val word_ptr_of_r32_plus_r32: [ `r32 of r32 ] -> [ `r32 of r32 ] -> [> `mem16 of mem ]

(** [reg + reg * scale] *)
val word_ptr_of_r32_plus_r32_scaled: [ `r32 of r32 ] -> [ `r32 of r32 ] -> int -> [> `mem16 of mem ]

(** [reg * scale] *)
val word_ptr_of_r32_scaled: [ `r32 of r32 ] -> int -> [> `mem16 of mem ]

(** [reg + offset] *)
val word_ptr_of_r32_plus_offset: [`r32 of r32 ] -> int -> [> `mem16 of mem ]

(** [reg + reg + offset] *)
val word_ptr_of_r32_plus_r32_plus_offset: [ `r32 of r32 ] -> [ `r32 of r32 ] -> int -> [> `mem16 of mem ]

(** [reg + reg * scale + offset] *)
val word_ptr_of_r32_plus_r32_scaled_plus_offset: [ `r32 of r32 ] -> [ `r32 of r32 ] -> int -> int -> [> `mem16 of mem ]

(** [reg * scale + offset] *)
val word_ptr_of_r32_scaled_plus_offset: [ `r32 of r32 ] -> int -> int -> [> `mem16 of mem ]

(** [reg] *)
val qword_ptr_of_r32: [ `r32 of r32 ] -> [> `mem64 of mem ]

(** [reg + reg] *)
val qword_ptr_of_r32_plus_r32: [ `r32 of r32 ] -> [ `r32 of r32 ] -> [> `mem64 of mem ]

(** [reg + reg * scale] *)
val qword_ptr_of_r32_plus_r32_scaled: [ `r32 of r32 ] -> [ `r32 of r32 ] -> int -> [> `mem64 of mem ]

(** [reg * scale] *)
val qword_ptr_of_r32_scaled: [ `r32 of r32 ] -> int -> [> `mem64 of mem ]

(** [reg + offset] *)
val qword_ptr_of_r32_plus_offset: [`r32 of r32 ] -> int -> [> `mem64 of mem ]

(** [reg + reg + offset] *)
val qword_ptr_of_r32_plus_r32_plus_offset: [ `r32 of r32 ] -> [ `r32 of r32 ] -> int -> [> `mem64 of mem ]

(** [reg + reg * scale + offset] *)
val qword_ptr_of_r32_plus_r32_scaled_plus_offset: [ `r32 of r32 ] -> [ `r32 of r32 ] -> int -> int -> [> `mem64 of mem ]

(** [reg * scale + offset] *)
val qword_ptr_of_r32_scaled_plus_offset: [ `r32 of r32 ] -> int -> int -> [> `mem64 of mem ]


val push: push_type -> instruction

val to_label : string -> [> `short_label of string ]
val to_label_long : string -> [> `long_label of string ]
val jmp: jmp_type -> instruction

class chasm_block : object
  method append : Chasm_types.instruction -> unit
  method as_int_list : int list
  method jmp : Chasm_types.jmp_type -> unit
  method label : string -> unit
  method push : Chasm_types.push_type -> unit
end
