open Chasm
open Chasm__Chasm_exceptions

let word_ptr_of_reg = function
  | `r32 r -> word_ptr_of_r32 (`r32 r)
  | `r64 r -> word_ptr_of_r64 (`r64 r)

let qword_ptr_of_reg = function
  | `r32 r -> qword_ptr_of_r32 (`r32 r)
  | `r64 r -> qword_ptr_of_r64 (`r64 r)

let word_ptr_of_reg_plus_reg base index = match base, index with
  | `r32 b, `r32 i -> word_ptr_of_r32_plus_r32 (`r32 b) (`r32 i)
  | `r64 b, `r64 i -> word_ptr_of_r64_plus_r64 (`r64 b) (`r64 i)
  | _ -> raise (Invalid_encoding "Both regs must be the same type")

let qword_ptr_of_reg_plus_reg base index = match base, index with
  | `r32 b, `r32 i -> qword_ptr_of_r32_plus_r32 (`r32 b) (`r32 i)
  | `r64 b, `r64 i -> qword_ptr_of_r64_plus_r64 (`r64 b) (`r64 i)
  | _ -> raise (Invalid_encoding "Both regs must be the same type")

let word_ptr_of_reg_scaled base scale = match base with
  | `r32 b -> word_ptr_of_r32_scaled (`r32 b) scale
  | `r64 b -> word_ptr_of_r64_scaled (`r64 b) scale

let qword_ptr_of_reg_scaled base scale = match base with
  | `r32 b -> qword_ptr_of_r32_scaled (`r32 b) scale
  | `r64 b -> qword_ptr_of_r64_scaled (`r64 b) scale

let word_ptr_of_reg_plus_reg_scaled base index scale = match base, index with
  | `r32 base, `r32 index -> word_ptr_of_r32_plus_r32_scaled (`r32 base) (`r32 index) scale
  | `r64 base, `r64 index -> word_ptr_of_r64_plus_r64_scaled (`r64 base) (`r64 index) scale
  | _ -> raise (Invalid_encoding "Both regs must be the same type")

let qword_ptr_of_reg_plus_reg_scaled base index scale = match base, index with
  | `r32 base, `r32 index -> qword_ptr_of_r32_plus_r32_scaled (`r32 base) (`r32 index) scale
  | `r64 base, `r64 index -> qword_ptr_of_r64_plus_r64_scaled (`r64 base) (`r64 index) scale
  | _ -> raise (Invalid_encoding "Both regs must be the same type")

let word_ptr_of_reg_plus_reg_plus_offset base index offset = match base, index with
  | `r32 base, `r32 index -> word_ptr_of_r32_plus_r32_plus_offset (`r32 base) (`r32 index) offset
  | `r64 base, `r64 index -> word_ptr_of_r64_plus_r64_plus_offset (`r64 base) (`r64 index) offset
  | _ -> raise (Invalid_encoding "Both regs must be the same type")

let qword_ptr_of_reg_plus_reg_plus_offset base index offset = match base, index with
  | `r32 base, `r32 index -> qword_ptr_of_r32_plus_r32_plus_offset (`r32 base) (`r32 index) offset
  | `r64 base, `r64 index -> qword_ptr_of_r64_plus_r64_plus_offset (`r64 base) (`r64 index) offset
  | _ -> raise (Invalid_encoding "Both regs must be the same type")

let word_ptr_of_reg_plus_offset reg offset = match reg with
  | `r32 reg -> word_ptr_of_r32_plus_offset (`r32 reg) offset
  | `r64 reg -> word_ptr_of_r64_plus_offset (`r64 reg) offset

let qword_ptr_of_reg_plus_offset reg offset = match reg with
  | `r32 reg -> qword_ptr_of_r32_plus_offset (`r32 reg) offset
  | `r64 reg -> qword_ptr_of_r64_plus_offset (`r64 reg) offset

let word_ptr_of_reg_plus_reg_scaled_plus_offset base index scale offset = match base, index with
  | `r32 base, `r32 index -> word_ptr_of_r32_plus_r32_scaled_plus_offset (`r32 base) (`r32 index) scale offset
  | `r64 base, `r64 index -> word_ptr_of_r64_plus_r64_scaled_plus_offset (`r64 base) (`r64 index) scale offset
  | _ -> raise (Invalid_encoding "Both regs must be the same type")

let qword_ptr_of_reg_plus_reg_scaled_plus_offset base index scale offset = match base, index with
  | `r32 base, `r32 index -> qword_ptr_of_r32_plus_r32_scaled_plus_offset (`r32 base) (`r32 index) scale offset
  | `r64 base, `r64 index -> qword_ptr_of_r64_plus_r64_scaled_plus_offset (`r64 base) (`r64 index) scale offset
  | _ -> raise (Invalid_encoding "Both regs must be the same type")