open Chasm_types
open Chasm_util
open Stdint

let al   = `r8 Al
let cl   = `r8 Cl
let dl   = `r8 Dl
let bl   = `r8 Bl
let sil  = `r8 Sil
let dil  = `r8 Dil
let spl  = `r8 Spl
let bpl  = `r8 Bpl
let r8b  = `r8 R8b
let r9b  = `r8 R9b
let r10b = `r8 R10b
let r11b = `r8 R11b
let r12b = `r8 R12b
let r13b = `r8 R13b
let r14b = `r8 R14b
let r15b = `r8 R15b

let ax   = `r16 Ax
let cx   = `r16 Cx
let dx   = `r16 Dx
let bx   = `r16 Bx
let si   = `r16 Si
let di   = `r16 Di
let sp   = `r16 Sp
let bp   = `r16 Bp
let r8w  = `r16 R8w
let r9w  = `r16 R9w
let r10w = `r16 R10w
let r11w = `r16 R11w
let r12w = `r16 R12w
let r13w = `r16 R13w
let r14w = `r16 R14w
let r15w = `r16 R15w

let eax  = `r32 Eax
let ecx  = `r32 Ecx
let edx  = `r32 Edx
let ebx  = `r32 Ebx
let esi  = `r32 Esi
let edi  = `r32 Edi
let esp  = `r32 Esp
let ebp  = `r32 Ebp
let r8d  = `r32 R8d
let r9d  = `r32 R9d
let r10d = `r32 R10d
let r11d = `r32 R11d
let r12d = `r32 R12d
let r13d = `r32 R13d
let r14d = `r32 R14d
let r15d = `r32 R15d

let rax = `r64 Rax
let rcx = `r64 Rcx
let rdx = `r64 Rdx
let rbx = `r64 Rbx
let rsi = `r64 Rsi
let rdi = `r64 Rdi
let rsp = `r64 Rsp
let rbp = `r64 Rbp
let r8  = `r64 R8
let r9  = `r64 R9
let r10 = `r64 R10
let r11 = `r64 R11
let r12 = `r64 R12
let r13 = `r64 R13
let r14 = `r64 R14
let r15 = `r64 R15

let rb = function
  | 0 -> al  | 1 -> cl  | 2 -> dl    | 3 -> bl    | 4 -> spl   | 5 -> bpl   | 6 -> sil   | 7 -> dil
  | 8 -> r8b | 9 -> r9b | 10 -> r10b | 11 -> r11b | 12 -> r12b | 13 -> r13b | 14 -> r14b | 15 -> r15b
  | x -> raise (Invalid_argument (Printf.sprintf "Invalid register: %d valid values are 0-15" x))

let rw = function
  | 0 ->  ax  | 1 ->  cx  | 2 ->  dx   | 3 ->  bx   | 4 ->  sp   | 5 ->  bp   | 6 ->  si   | 7 ->  di
  | 8 ->  r8w | 9 ->  r9w | 10 -> r10w | 11 -> r11w | 12 -> r12w | 13 -> r13w | 14 -> r14w | 15 -> r15w
  | x -> raise (Invalid_argument (Printf.sprintf "Invalid register: %d valid values are 0-15" x))

let rd = function
  | 0 ->  eax | 1 ->  ecx | 2 ->  edx  | 3 ->  ebx  | 4 ->  esp  | 5 ->  ebp  | 6 ->  esi  | 7 ->  edi
  | 8 ->  r8d | 9 ->  r9d | 10 -> r10d | 11 -> r11d | 12 -> r12d | 13 -> r13d | 14 -> r14d | 15 -> r15d
  | x -> raise (Invalid_argument (Printf.sprintf "Invalid register: %d valid values are 0-15" x))

let rq = function
  | 0 ->  rax | 1 ->  rcx | 2 ->  rdx | 3 ->  rbx | 4 ->  rsp | 5 ->  rbp | 6 ->  rsi | 7 ->  rdi
  | 8 ->  r8  | 9 ->  r9  | 10 -> r10 | 11 -> r11 | 12 -> r12 | 13 -> r13 | 14 -> r14 | 15 -> r15
  | x -> raise (Invalid_argument (Printf.sprintf "Invalid register: %d valid values are 0-15" x))

let imm   x = `imm x
let imm8  x = `imm8 x
let imm8_i x = imm8 (Int8.of_int x)
let imm16 x = `imm16 x
let imm16_i x = imm16 (Int16.of_int x)
let imm32 x = `imm32 x
let imm32_i x = imm32 (Int32.of_int x)
let imm64 x = `imm64 x

let push x = Push x

let assemble_push_mem size mem = match validate_mem mem with 
    | R64Ptr m ->
      let base_num, index_num = (Option.map r64_to_int m.base), (Option.map r64_to_int m.index) in
        let rex = make_rex_bx_opts base_num index_num in
          let modbits, offset = get_modbits_and_offset m.base m.offset in
            let sib = make_sib_opts m.scale index_num base_num in 
              let rmbits = if (Option.is_some sib) then 4 else (Option.get base_num) in (* 4 signals a present SIB byte. Otherwise, a base reg is required. *)
                let modrm = make_modrm modbits 6 rmbits in
                  let size_prefix = if (size = M16) then (Some prefix_op_size_override) else None in
                    make_bytes (size_prefix +? (rex +? ([0xFF; modrm] @? sib) @ offset))


let rec assemble = function
  | Push (`r16 r)   -> let reg_num = rw_to_int(`r16 r) in
                        let rex = make_rex_b reg_num in
                          make_bytes (prefix_op_size_override :: rex +? [0x50 + (reg_num land 7)])

  | Push (`r64 r)   -> let reg_num = rq_to_int(`r64 r) in
                        let rex = make_rex_b reg_num in
                          make_bytes (rex +? [0x50 + (reg_num land 7)])

  | Push (`imm8  i) -> make_bytes [0x6A; Int8.to_int i]
  | Push (`imm16 i) -> make_bytes ([0x66; 0x68] @ (list_of_int16_le i))
  | Push (`imm32 i) -> make_bytes (0x68 :: (list_of_int32_le i))
  | Push (`imm i)   -> assemble (Push (int_to_sized_imm i))
  | Push (`mem16 m) -> assemble_push_mem M16 m
  | Push (`mem64 m) -> assemble_push_mem M64 m


let offset_of_int = function
      | o when o == 0 -> None
      | o -> Some (`imm o)

let word_ptr_of_r64_plus_offset base offset = match base, offset with
      | `r64 base, offset ->
        `mem16 (R64Ptr { base = Some base; index = None; scale = None; offset = offset_of_int offset})

let word_ptr_of_r64_plus_r64_plus_offset base index offset = match base, index, offset with
    (* rsp is invalid in the index field, so quietly swap it to base if possible *)
    | `r64 base, `r64 index, offset when index = Rsp && base <> Rsp -> 
      `mem16 (R64Ptr { base = Some index; index = Some base; scale = None; offset = offset_of_int offset})

    | `r64 base, `r64 index, offset -> 
      `mem16 (R64Ptr { base = Some base; index = Some index; scale = None; offset = offset_of_int offset})

let word_ptr_of_r64_plus_r64_scaled_plus_offset base index scale offset = match base, index, scale, offset with
    | `r64 base, `r64 index, scale, offset when scale == 1 -> 
      (* If scale is 1, call back to normal reg + reg function so that the rsp as index case can be handled there *)
      word_ptr_of_r64_plus_r64_plus_offset (`r64 base) (`r64 index) offset

    | `r64 base, `r64 index, scale, offset -> 
      `mem16 (R64Ptr { base = Some base; index = Some index; scale = Some scale; offset = offset_of_int offset})

let word_ptr_of_r64_scaled_plus_offset base scale offset = match base, scale, offset with
    (* When the scaling factor is 1, no need to encode base in the index field *)
    | `r64 base, scale, offset when scale == 1 -> word_ptr_of_r64_plus_offset (`r64 base) offset

    | `r64 base, scale, offset -> 
      `mem16 (R64Ptr { base = None; index = Some base; scale = Some scale; offset = offset_of_int offset})

let word_ptr_of_r64 base = word_ptr_of_r64_plus_offset base 0
let word_ptr_of_r64_plus_r64 base index = word_ptr_of_r64_plus_r64_plus_offset base index 0
let word_ptr_of_r64_plus_r64_scaled base index scale = word_ptr_of_r64_plus_r64_scaled_plus_offset base index scale 0
let word_ptr_of_r64_scaled base scale = word_ptr_of_r64_scaled_plus_offset base scale 0

let qword_ptr_of_r64_plus_offset base offset = match base, offset with
      | `r64 base, offset ->
        `mem64 (R64Ptr { base = Some base; index = None; scale = None; offset = offset_of_int offset})

let qword_ptr_of_r64_plus_r64_plus_offset base index offset = match base, index, offset with
    (* rsp is invalid in the index field, so quietly swap it to base if possible *)
    | `r64 base, `r64 index, offset when index = Rsp && base <> Rsp -> 
      `mem64 (R64Ptr { base = Some index; index = Some base; scale = None; offset = offset_of_int offset})

    | `r64 base, `r64 index, offset -> 
      `mem64 (R64Ptr { base = Some base; index = Some index; scale = None; offset = offset_of_int offset})

let qword_ptr_of_r64_plus_r64_scaled_plus_offset base index scale offset = match base, index, scale, offset with
    | `r64 base, `r64 index, scale, offset when scale == 1 -> 
      (* If scale is 1, call back to normal reg + reg function so that the rsp as index case can be handled there *)
      qword_ptr_of_r64_plus_r64_plus_offset (`r64 base) (`r64 index) offset

    | `r64 base, `r64 index, scale, offset -> 
      `mem64 (R64Ptr { base = Some base; index = Some index; scale = Some scale; offset = offset_of_int offset})

let qword_ptr_of_r64_scaled_plus_offset base scale offset = match base, scale, offset with
    (* When the scaling factor is 1, no need to encode base in the index field *)
    | `r64 base, scale, offset when scale == 1 -> qword_ptr_of_r64_plus_offset (`r64 base) offset

    | `r64 base, scale, offset -> 
      `mem64 (R64Ptr { base = None; index = Some base; scale = Some scale; offset = offset_of_int offset})

let qword_ptr_of_r64 base = qword_ptr_of_r64_plus_offset base 0
let qword_ptr_of_r64_plus_r64 base index = qword_ptr_of_r64_plus_r64_plus_offset base index 0
let qword_ptr_of_r64_plus_r64_scaled base index scale = qword_ptr_of_r64_plus_r64_scaled_plus_offset base index scale 0
let qword_ptr_of_r64_scaled base scale = qword_ptr_of_r64_scaled_plus_offset base scale 0

let assemble_list instrs = let asm = List.map assemble instrs in
  Bytes.concat Bytes.empty asm