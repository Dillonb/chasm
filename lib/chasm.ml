open Chasm_types
open Chasm_util
open Chasm_exceptions
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

let rb_notag = function
  | 0 -> Al  | 1 -> Cl  | 2 -> Dl    | 3 -> Bl    | 4 -> Spl   | 5 -> Bpl   | 6 -> Sil   | 7 -> Dil
  | 8 -> R8b | 9 -> R9b | 10 -> R10b | 11 -> R11b | 12 -> R12b | 13 -> R13b | 14 -> R14b | 15 -> R15b
  | x -> raise (Invalid_argument (Printf.sprintf "Invalid register: %d valid values are 0-15" x))

let rw_notag = function
  | 0 ->  Ax  | 1 ->  Cx  | 2 ->  Dx   | 3 ->  Bx   | 4 ->  Sp   | 5 ->  Bp   | 6 ->  Si   | 7 ->  Di
  | 8 ->  R8w | 9 ->  R9w | 10 -> R10w | 11 -> R11w | 12 -> R12w | 13 -> R13w | 14 -> R14w | 15 -> R15w
  | x -> raise (Invalid_argument (Printf.sprintf "Invalid register: %d valid values are 0-15" x))

let rd_notag = function
  | 0 ->  Eax | 1 ->  Ecx | 2 ->  Edx  | 3 ->  Ebx  | 4 ->  Esp  | 5 ->  Ebp  | 6 ->  Esi  | 7 ->  Edi
  | 8 ->  R8d | 9 ->  R9d | 10 -> R10d | 11 -> R11d | 12 -> R12d | 13 -> R13d | 14 -> R14d | 15 -> R15d
  | x -> raise (Invalid_argument (Printf.sprintf "Invalid register: %d valid values are 0-15" x))

let rq_notag = function
  | 0 ->  Rax | 1 ->  Rcx | 2 ->  Rdx | 3 ->  Rbx | 4 ->  Rsp | 5 ->  Rbp | 6 ->  Rsi | 7 ->  Rdi
  | 8 ->  R8  | 9 ->  R9  | 10 -> R10 | 11 -> R11 | 12 -> R12 | 13 -> R13 | 14 -> R14 | 15 -> R15
  | x -> raise (Invalid_argument (Printf.sprintf "Invalid register: %d valid values are 0-15" x))

let imm   x = `imm x
let imm8  x = `imm8 x
let imm8_i x = if is_int8 x then imm8 (Int8.of_int x) else raise (Invalid_argument (Printf.sprintf "Invalid imm8: %d valid values are -128-127" x))
let uimm8  x = `uimm8 x
let uimm8_i x = if is_uint8 x then uimm8 (Uint8.of_int x) else raise (Invalid_argument (Printf.sprintf "Invalid uimm8: %d valid values are 0-255" x))
let imm16 x = `imm16 x
let imm16_i x = if is_int16 x then imm16 (Int16.of_int x) else raise (Invalid_argument (Printf.sprintf "Invalid imm16: %d valid values are -32768-32767" x))
let uimm16 x = `uimm16 x
let uimm16_i x = if is_uint16 x then uimm16 (Uint16.of_int x) else raise (Invalid_argument (Printf.sprintf "Invalid uimm16: %d valid values are 0-65535" x))
let imm32 x = `imm32 x
let imm32_i x = if is_int32 x then imm32 (Int32.of_int x) else raise (Invalid_argument (Printf.sprintf "Invalid imm32: %d valid values are -2147483648-2147483647" x))
let uimm32 x = `uimm32 x
let uimm32_i x = if is_uint32 x then uimm32 (Uint32.of_int x) else raise (Invalid_argument (Printf.sprintf "Invalid uimm32: %d valid values are 0-4294967295" x))
let imm64 x = `imm64 x

let push x = Push x
let jmp x = Jmp x
let sub x y = Sub (x,y)

let assemble_modrm_mem_op opbyte regbits size mem = 
  let do_assemble m base_num index_num prefix = 
    let rex = make_rex_bx_opts base_num index_num in
      let modbits, offset = get_modbits_and_offset base_num m.offset in
        let sib = make_sib_opts m.scale index_num base_num in 
          let rmbits = if (Option.is_some sib) then 4 else (Option.get base_num) in (* 4 signals a present SIB byte. Otherwise, a base reg is required. *)
            let modrm = make_modrm modbits regbits rmbits in
              let size_prefix = if (size = M16) then (Some prefix_op_size_override) else None in
                (prefix +? (size_prefix +? (rex +? (opbyte :: (modrm :: (sib +? offset)))))) in
  match validate_mem mem with 
    | R64Ptr m ->
      let base_num, index_num = (Option.map r64_to_int m.base), (Option.map r64_to_int m.index) in
        do_assemble m base_num index_num None
    | R32Ptr m ->
      let base_num, index_num = (Option.map r32_to_int m.base), (Option.map r32_to_int m.index) in
        do_assemble m base_num index_num (Some prefix_addr_size_override)

let assemble_modrm_reg_op rex opbyte regbits reg =
  let do_assemble reg_num = 
    let modrm = make_modrm 3 regbits reg_num in
      let rex = combine_rex rex (make_rex_reg reg) in
        rex +? (opbyte :: [modrm]) in
  match reg with
    | `r8 r -> do_assemble (rb_to_int (`r8 r))
    | `r32 r -> do_assemble (rd_to_int (`r32 r))
    | `r64 r -> do_assemble (rq_to_int (`r64 r))

type imm_size =
 | Imm8
 | Imm32

type needs_label = {
    data: int list;
    label: string;

    (* offset of the instruction in the code *)
    offset: int;

    (* Offset from which the relative offset should be calculated from *)
    relative_offset_base: int;

    (* offset within the instruction of the immediate to be rewritten later *)
    imm_offset: int;
    (* size of the immediate *)
    imm_size: imm_size;
}

type assembled_instruction = 
  | Complete of int list
  | NeedsLabel of needs_label

let resolve_label label instruction_offset labels = 
  let maybe_label_offset = Hashtbl.find_opt labels label in
  Option.map (fun label_offset -> label_offset - instruction_offset) maybe_label_offset

let rec assemble instruction instruction_offset labels = match instruction with
  | Push (`r16 r)   -> let reg_num = rw_to_int(`r16 r) in
                        let rex = make_rex_b reg_num in
                          Complete (prefix_op_size_override :: rex +? [0x50 + (reg_num land 7)])

  | Push (`r64 r)   -> let reg_num = rq_to_int(`r64 r) in
                        let rex = make_rex_b reg_num in
                          Complete (rex +? [0x50 + (reg_num land 7)])

  | Push (`imm8  i) -> Complete [0x6A; Int8.to_int i]
  | Push (`imm16 i) -> Complete ([0x66; 0x68] @ (list_of_int16_le i))
  | Push (`imm32 i) -> Complete (0x68 :: (list_of_int32_le i))
  | Push (`imm i)   -> assemble (Push (int_to_sized_imm i)) instruction_offset labels
  | Push (`mem16 m) -> Complete (assemble_modrm_mem_op 0xFF 6 M16 m)
  | Push (`mem64 m) -> Complete (assemble_modrm_mem_op 0xFF 6 M64 m)
  | Push (_) -> raise (Invalid_encoding "Invalid Push encoding")

  | Jmp (`imm8 i) -> Complete [0xEB; Int8.to_int i]
  | Jmp (`imm32 i) -> Complete (0xE9 :: (list_of_int32_le i))

  | Jmp (`mem64 m) -> Complete (assemble_modrm_mem_op 0xFF 4 M64 m)

  | Jmp (`short_label label) -> (
    let jump_origin_offset = instruction_offset + 2 in
      match resolve_label label (jump_origin_offset) labels with
      | Some jump_offset when is_int8 jump_offset -> assemble (Jmp (imm8_i jump_offset)) jump_origin_offset labels
      | Some jump_offset -> raise (Invalid_encoding ("Label is too far away (offset is " ^ (string_of_int jump_offset) ^ " bytes) for an 8 bit offset")) 
      | None -> NeedsLabel { data=[0xEB; 0]; label=label; offset=instruction_offset; relative_offset_base=jump_origin_offset; imm_size=Imm8; imm_offset=1;}
  )
    
  | Jmp (`long_label label) -> (
    let jump_origin_offset = instruction_offset + 5 in
      match resolve_label label jump_origin_offset labels with
      | Some jump_offset when is_int32 jump_offset -> assemble (Jmp (imm32_i jump_offset)) jump_origin_offset labels
      | Some jump_offset -> raise (Invalid_encoding ("Label is too far away (offset is " ^ (string_of_int jump_offset) ^ " bytes) for a 32 bit offset")) 
      | None -> NeedsLabel { data=[0xE9; 0; 0; 0; 0]; label=label; offset=instruction_offset; relative_offset_base=jump_origin_offset; imm_size=Imm32; imm_offset=1;}
  )
  | Jmp (_) -> raise (Invalid_encoding "Invalid or unhandled jmp encoding")

  | Sub(`r8  Al,  `imm i) when is_uint8  i -> Complete [0x2C; i]
  | Sub(`r16 Ax,  `imm i) when is_uint16 i -> Complete (prefix_op_size_override :: 0x2D :: (list_of_uint16_le_i i))
  | Sub(`r32 Eax, `imm i) when is_uint32 i -> Complete (0x2D :: (list_of_uint32_le_i i))
  | Sub(`r64 Rax, `imm i) when is_int32  i -> Complete (rex_w :: 0x2D :: (list_of_int32_le_i i))

  | Sub(`r8 r,  `imm i) when is_uint8 i -> Complete ((assemble_modrm_reg_op None         0x80 5 (`r8 r))  @ [i])
  | Sub(`r32 r, `imm i) when is_int8  i -> Complete ((assemble_modrm_reg_op None         0x83 5 (`r32 r)) @ [i])
  | Sub(`r64 r, `imm i) when is_int8  i -> Complete ((assemble_modrm_reg_op (Some rex_w) 0x83 5 (`r64 r)) @ [i])

  (* todo: remove sized imm parameters entirely *)
  | Sub(arg, `uimm8 i)  -> assemble (Sub(arg, `imm (Uint8.to_int i)))  instruction_offset labels
  | Sub(arg, `uimm16 i) -> assemble (Sub(arg, `imm (Uint16.to_int i))) instruction_offset labels
  | Sub(arg, `uimm32 i) -> assemble (Sub(arg, `imm (Uint32.to_int i))) instruction_offset labels
  | Sub(arg, `imm32 i)  -> assemble (Sub(arg, `imm (Int32.to_int i)))  instruction_offset labels
  | Sub(_) -> raise (Not_implemented "Invalid or unhandled sub encoding")

  | Mov(`r64 r, `imm64 i) -> Complete ((combine_rex (Some rex_w) (make_rex_reg (`r64 r))) +? (0xB8 + ((r64_to_int r) land 7) :: (list_of_uint64_le i)))
  | Mov(_) -> raise (Not_implemented "Invalid or unhandled mov encoding")

let offset_of_int = function
      | o when o == 0 -> None
      | o -> Some (`imm o)

let byte_ptr_of_r64_plus_offset base offset = match base, offset with
      | `r64 base, offset ->
        `mem8 (R64Ptr { base = Some base; index = None; scale = None; offset = offset_of_int offset})

let byte_ptr_of_r64_plus_r64_plus_offset base index offset = match base, index, offset with
    (* rsp is invalid in the index field, so quietly swap it to base if possible *)
    | `r64 base, `r64 index, offset when index = Rsp && base <> Rsp -> 
      `mem8 (R64Ptr { base = Some index; index = Some base; scale = None; offset = offset_of_int offset})

    | `r64 base, `r64 index, offset -> 
      `mem8 (R64Ptr { base = Some base; index = Some index; scale = None; offset = offset_of_int offset})

let byte_ptr_of_r64_plus_r64_scaled_plus_offset base index scale offset = match base, index, scale, offset with
    | `r64 base, `r64 index, scale, offset when scale == 1 -> 
      (* If scale is 1, call back to normal reg + reg function so that the rsp as index case can be handled there *)
      byte_ptr_of_r64_plus_r64_plus_offset (`r64 base) (`r64 index) offset

    | `r64 base, `r64 index, scale, offset -> 
      `mem8 (R64Ptr { base = Some base; index = Some index; scale = Some scale; offset = offset_of_int offset})

let byte_ptr_of_r64_scaled_plus_offset base scale offset = match base, scale, offset with
    (* When the scaling factor is 1, no need to encode base in the index field *)
    | `r64 base, scale, offset when scale == 1 -> byte_ptr_of_r64_plus_offset (`r64 base) offset

    | `r64 base, scale, offset -> 
      `mem8 (R64Ptr { base = None; index = Some base; scale = Some scale; offset = offset_of_int offset})

let byte_ptr_of_r64 base = byte_ptr_of_r64_plus_offset base 0
let byte_ptr_of_r64_plus_r64 base index = byte_ptr_of_r64_plus_r64_plus_offset base index 0
let byte_ptr_of_r64_plus_r64_scaled base index scale = byte_ptr_of_r64_plus_r64_scaled_plus_offset base index scale 0
let byte_ptr_of_r64_scaled base scale = byte_ptr_of_r64_scaled_plus_offset base scale 0

let byte_ptr_of_r32_plus_offset base offset = match base, offset with
      | `r32 base, offset ->
        `mem8 (R32Ptr { base = Some base; index = None; scale = None; offset = offset_of_int offset})

let byte_ptr_of_r32_plus_r32_plus_offset base index offset = match base, index, offset with
    (* rsp is invalid in the index field, so quietly swap it to base if possible *)
    | `r32 base, `r32 index, offset when index = Esp && base <> Esp -> 
      `mem8 (R32Ptr { base = Some index; index = Some base; scale = None; offset = offset_of_int offset})

    | `r32 base, `r32 index, offset -> 
      `mem8 (R32Ptr { base = Some base; index = Some index; scale = None; offset = offset_of_int offset})

let byte_ptr_of_r32_plus_r32_scaled_plus_offset base index scale offset = match base, index, scale, offset with
    | `r32 base, `r32 index, scale, offset when scale == 1 -> 
      (* If scale is 1, call back to normal reg + reg function so that the rsp as index case can be handled there *)
      byte_ptr_of_r32_plus_r32_plus_offset (`r32 base) (`r32 index) offset

    | `r32 base, `r32 index, scale, offset -> 
      `mem8 (R32Ptr { base = Some base; index = Some index; scale = Some scale; offset = offset_of_int offset})

let byte_ptr_of_r32_scaled_plus_offset base scale offset = match base, scale, offset with
    (* When the scaling factor is 1, no need to encode base in the index field *)
    | `r32 base, scale, offset when scale == 1 -> byte_ptr_of_r32_plus_offset (`r32 base) offset

    | `r32 base, scale, offset -> 
      `mem8 (R32Ptr { base = None; index = Some base; scale = Some scale; offset = offset_of_int offset})

let byte_ptr_of_r32 base = byte_ptr_of_r32_plus_offset base 0
let byte_ptr_of_r32_plus_r32 base index = byte_ptr_of_r32_plus_r32_plus_offset base index 0
let byte_ptr_of_r32_plus_r32_scaled base index scale = byte_ptr_of_r32_plus_r32_scaled_plus_offset base index scale 0
let byte_ptr_of_r32_scaled base scale = byte_ptr_of_r32_scaled_plus_offset base scale 0

(* STOP HERE *)

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

let word_ptr_of_r32_plus_offset base offset = match base, offset with
      | `r32 base, offset ->
        `mem16 (R32Ptr { base = Some base; index = None; scale = None; offset = offset_of_int offset})

let word_ptr_of_r32_plus_r32_plus_offset base index offset = match base, index, offset with
    (* rsp is invalid in the index field, so quietly swap it to base if possible *)
    | `r32 base, `r32 index, offset when index = Esp && base <> Esp -> 
      `mem16 (R32Ptr { base = Some index; index = Some base; scale = None; offset = offset_of_int offset})

    | `r32 base, `r32 index, offset -> 
      `mem16 (R32Ptr { base = Some base; index = Some index; scale = None; offset = offset_of_int offset})

let word_ptr_of_r32_plus_r32_scaled_plus_offset base index scale offset = match base, index, scale, offset with
    | `r32 base, `r32 index, scale, offset when scale == 1 -> 
      (* If scale is 1, call back to normal reg + reg function so that the rsp as index case can be handled there *)
      word_ptr_of_r32_plus_r32_plus_offset (`r32 base) (`r32 index) offset

    | `r32 base, `r32 index, scale, offset -> 
      `mem16 (R32Ptr { base = Some base; index = Some index; scale = Some scale; offset = offset_of_int offset})

let word_ptr_of_r32_scaled_plus_offset base scale offset = match base, scale, offset with
    (* When the scaling factor is 1, no need to encode base in the index field *)
    | `r32 base, scale, offset when scale == 1 -> word_ptr_of_r32_plus_offset (`r32 base) offset

    | `r32 base, scale, offset -> 
      `mem16 (R32Ptr { base = None; index = Some base; scale = Some scale; offset = offset_of_int offset})

let word_ptr_of_r32 base = word_ptr_of_r32_plus_offset base 0
let word_ptr_of_r32_plus_r32 base index = word_ptr_of_r32_plus_r32_plus_offset base index 0
let word_ptr_of_r32_plus_r32_scaled base index scale = word_ptr_of_r32_plus_r32_scaled_plus_offset base index scale 0
let word_ptr_of_r32_scaled base scale = word_ptr_of_r32_scaled_plus_offset base scale 0

let dword_ptr_of_r64_plus_offset base offset = match base, offset with
      | `r64 base, offset ->
        `mem32 (R64Ptr { base = Some base; index = None; scale = None; offset = offset_of_int offset})

let dword_ptr_of_r64_plus_r64_plus_offset base index offset = match base, index, offset with
    (* rsp is invalid in the index field, so quietly swap it to base if possible *)
    | `r64 base, `r64 index, offset when index = Rsp && base <> Rsp -> 
      `mem32 (R64Ptr { base = Some index; index = Some base; scale = None; offset = offset_of_int offset})

    | `r64 base, `r64 index, offset -> 
      `mem32 (R64Ptr { base = Some base; index = Some index; scale = None; offset = offset_of_int offset})

let dword_ptr_of_r64_plus_r64_scaled_plus_offset base index scale offset = match base, index, scale, offset with
    | `r64 base, `r64 index, scale, offset when scale == 1 -> 
      (* If scale is 1, call back to normal reg + reg function so that the rsp as index case can be handled there *)
      dword_ptr_of_r64_plus_r64_plus_offset (`r64 base) (`r64 index) offset

    | `r64 base, `r64 index, scale, offset -> 
      `mem32 (R64Ptr { base = Some base; index = Some index; scale = Some scale; offset = offset_of_int offset})

let dword_ptr_of_r64_scaled_plus_offset base scale offset = match base, scale, offset with
    (* When the scaling factor is 1, no need to encode base in the index field *)
    | `r64 base, scale, offset when scale == 1 -> dword_ptr_of_r64_plus_offset (`r64 base) offset

    | `r64 base, scale, offset -> 
      `mem32 (R64Ptr { base = None; index = Some base; scale = Some scale; offset = offset_of_int offset})

let dword_ptr_of_r64 base = dword_ptr_of_r64_plus_offset base 0
let dword_ptr_of_r64_plus_r64 base index = dword_ptr_of_r64_plus_r64_plus_offset base index 0
let dword_ptr_of_r64_plus_r64_scaled base index scale = dword_ptr_of_r64_plus_r64_scaled_plus_offset base index scale 0
let dword_ptr_of_r64_scaled base scale = dword_ptr_of_r64_scaled_plus_offset base scale 0

let dword_ptr_of_r32_plus_offset base offset = match base, offset with
      | `r32 base, offset ->
        `mem32 (R32Ptr { base = Some base; index = None; scale = None; offset = offset_of_int offset})

let dword_ptr_of_r32_plus_r32_plus_offset base index offset = match base, index, offset with
    (* rsp is invalid in the index field, so quietly swap it to base if possible *)
    | `r32 base, `r32 index, offset when index = Esp && base <> Esp -> 
      `mem32 (R32Ptr { base = Some index; index = Some base; scale = None; offset = offset_of_int offset})

    | `r32 base, `r32 index, offset -> 
      `mem32 (R32Ptr { base = Some base; index = Some index; scale = None; offset = offset_of_int offset})

let dword_ptr_of_r32_plus_r32_scaled_plus_offset base index scale offset = match base, index, scale, offset with
    | `r32 base, `r32 index, scale, offset when scale == 1 -> 
      (* If scale is 1, call back to normal reg + reg function so that the rsp as index case can be handled there *)
      dword_ptr_of_r32_plus_r32_plus_offset (`r32 base) (`r32 index) offset

    | `r32 base, `r32 index, scale, offset -> 
      `mem32 (R32Ptr { base = Some base; index = Some index; scale = Some scale; offset = offset_of_int offset})

let dword_ptr_of_r32_scaled_plus_offset base scale offset = match base, scale, offset with
    (* When the scaling factor is 1, no need to encode base in the index field *)
    | `r32 base, scale, offset when scale == 1 -> dword_ptr_of_r32_plus_offset (`r32 base) offset

    | `r32 base, scale, offset -> 
      `mem32 (R32Ptr { base = None; index = Some base; scale = Some scale; offset = offset_of_int offset})

let dword_ptr_of_r32 base = dword_ptr_of_r32_plus_offset base 0
let dword_ptr_of_r32_plus_r32 base index = dword_ptr_of_r32_plus_r32_plus_offset base index 0
let dword_ptr_of_r32_plus_r32_scaled base index scale = dword_ptr_of_r32_plus_r32_scaled_plus_offset base index scale 0
let dword_ptr_of_r32_scaled base scale = dword_ptr_of_r32_scaled_plus_offset base scale 0

(* STOP HERE *)

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

let qword_ptr_of_r32_plus_offset base offset = match base, offset with
      | `r32 base, offset ->
        `mem64 (R32Ptr { base = Some base; index = None; scale = None; offset = offset_of_int offset})

let qword_ptr_of_r32_plus_r32_plus_offset base index offset = match base, index, offset with
    (* rsp is invalid in the index field, so quietly swap it to base if possible *)
    | `r32 base, `r32 index, offset when index = Esp && base <> Esp -> 
      `mem64 (R32Ptr { base = Some index; index = Some base; scale = None; offset = offset_of_int offset})

    | `r32 base, `r32 index, offset -> 
      `mem64 (R32Ptr { base = Some base; index = Some index; scale = None; offset = offset_of_int offset})

let qword_ptr_of_r32_plus_r32_scaled_plus_offset base index scale offset = match base, index, scale, offset with
    | `r32 base, `r32 index, scale, offset when scale == 1 -> 
      (* If scale is 1, call back to normal reg + reg function so that the rsp as index case can be handled there *)
      qword_ptr_of_r32_plus_r32_plus_offset (`r32 base) (`r32 index) offset

    | `r32 base, `r32 index, scale, offset -> 
      `mem64 (R32Ptr { base = Some base; index = Some index; scale = Some scale; offset = offset_of_int offset})

let qword_ptr_of_r32_scaled_plus_offset base scale offset = match base, scale, offset with
    (* When the scaling factor is 1, no need to encode base in the index field *)
    | `r32 base, scale, offset when scale == 1 -> qword_ptr_of_r32_plus_offset (`r32 base) offset

    | `r32 base, scale, offset -> 
      `mem64 (R32Ptr { base = None; index = Some base; scale = Some scale; offset = offset_of_int offset})

let qword_ptr_of_r32 base = qword_ptr_of_r32_plus_offset base 0
let qword_ptr_of_r32_plus_r32 base index = qword_ptr_of_r32_plus_r32_plus_offset base index 0
let qword_ptr_of_r32_plus_r32_scaled base index scale = qword_ptr_of_r32_plus_r32_scaled_plus_offset base index scale 0
let qword_ptr_of_r32_scaled base scale = qword_ptr_of_r32_scaled_plus_offset base scale 0

let to_label name = `short_label name
let to_label_long name = `long_label name

let hashtbl_append t k v =
  let existing = Hashtbl.find_opt t k in
    let existing_list = Option.value existing ~default:[] in
      Hashtbl.replace t k (v :: existing_list)

let int_list_of_bytes b =
  let rec int_list_of_bytes_internal b len start_offset =
    if (start_offset >= len) then []
    else Bytes.get_uint8 b start_offset :: int_list_of_bytes_internal b len (start_offset + 1) 
  in int_list_of_bytes_internal b (Bytes.length b) 0

let block_initial_buf_size = 16
class chasm_block =
  object (self)
    val buf = ref (Bytes.create block_initial_buf_size)
    val code_len = ref 0

    val labels = Hashtbl.create 5
    val unbound_labels = Hashtbl.create 5

    method has_unbound_labels = Hashtbl.length unbound_labels <> 0

    (* To be called when a new label is added - so that we can be sure that !code_len contains the label's offset. *)
    method private backpatch bp_data =
      let label_offset = !code_len in 
        let relative_offset = label_offset - bp_data.relative_offset_base in
          match bp_data.imm_size with
            | Imm8 when is_int8 relative_offset -> Bytes.set_int8 !buf (bp_data.offset + bp_data.imm_offset) relative_offset
            | Imm8 -> raise (Invalid_encoding ("Label is too far away (offset is " ^ (string_of_int relative_offset) ^ " bytes) for an 8 bit offset"))
            | Imm32 when is_int32 relative_offset -> Bytes.set_int32_le !buf (bp_data.offset + bp_data.imm_offset) (Int32.of_int relative_offset)
            | Imm32 -> raise (Invalid_encoding ("Label is too far away (offset is " ^ (string_of_int relative_offset) ^ " bytes) for a 32 bit offset"))

    method label (label_name: string) = 
      Hashtbl.add labels label_name !code_len;
      match Hashtbl.find_opt unbound_labels label_name with
        | Some l -> 
          List.iter self#backpatch l; 
          Hashtbl.remove unbound_labels label_name
        | None -> ()

    method private append_list l = let list_len = List.length l in
      if (!code_len + list_len > (Bytes.length !buf)) then 
        buf := Bytes.extend !buf 0 (Bytes.length !buf); (* Double the size of buf *)
      List.iteri (fun i v -> Bytes.set_uint8 !buf (!code_len + i) v) l; 
      code_len := !code_len + list_len

    method append_instruction instr =
      let asm = assemble instr !code_len labels in
        match asm with
          | Complete l -> self#append_list l
          | NeedsLabel l -> 
            hashtbl_append unbound_labels l.label l;
            self#append_list l.data

    method private append_asmline line = match line with
      | Label l -> self#label l
      | Instruction i -> self#append_instruction i

    method append lines =
      List.iter self#append_asmline lines

    method private unbound_label_names_str =
      let seq_names = Hashtbl.to_seq_keys unbound_labels in
        Seq.fold_left (fun a b -> a ^ "'" ^ b ^ "' ") "Some labels are unbound: " seq_names

    method as_bytes = 
      if self#has_unbound_labels then raise (Invalid_encoding (self#unbound_label_names_str)) 
      else Bytes.sub !buf 0 !code_len

    method as_int_list = int_list_of_bytes self#as_bytes

    method push x = self#append_instruction (Push x)
    method jmp x = self#append_instruction (Jmp x)
    method sub arg1 arg2 = self#append_instruction (Sub (arg1, arg2))
  end