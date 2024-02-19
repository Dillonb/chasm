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

let assemble_push_modrm = function
  | SingleR64 r -> 
    let reg_num = rq_to_int (`r64 r) in 
      let rex = make_rex_b reg_num in
        let sib = if (((reg_num land 7) == 4)) then Some 0x24 else None in (* rsp/r12 need SIB byte *)
          let modbits, offset = if ((reg_num land 7) == 5) then (1, Some 0) else (0, None) in (* using the base pointer (or r13) requires an offset *)
            make_bytes (rex +? ([0xFF; (make_modrm modbits 6 reg_num)] @? sib) @? offset)

  | R64PlusR64 (base, index) -> 
    let base_num, index_num = rq_to_int (`r64 base), rq_to_int(`r64 index) in
      let rex = make_rex_bx base_num index_num in
        make_bytes (rex +? [0xFF; 0x34; make_sib 0 index_num base_num])

let rec assemble = function
  | Push (`r16 r)   -> let reg_num = rw_to_int(`r16 r) in
                        let rex = make_rex_b reg_num in
                          make_bytes (0x66 :: rex +? [0x50 + (reg_num land 7)])

  | Push (`r64 r)   -> let reg_num = rq_to_int(`r64 r) in
                        let rex = make_rex_b reg_num in
                          make_bytes (rex +? [0x50 + (reg_num land 7)])

  | Push (`imm8  i) -> make_bytes [0x6A; Int8.to_int i]
  | Push (`imm16 i) -> make_bytes ([0x66; 0x68] @ (list_of_int16_le i))
  | Push (`imm32 i) -> make_bytes (0x68 :: (list_of_int32_le i))
  | Push (`imm i)   -> assemble (Push (int_to_sized_imm i))
  | Push (`modrm m) -> assemble_push_modrm m

let assemble_list instrs = let asm = List.map assemble instrs in
  Bytes.concat Bytes.empty asm

class mem_op_base_plus_reg (base_reg, ofs_reg) = object
  val base = base_reg
  val ofs = ofs_reg
  method build : 'a. [> `modrm of modrm ] as 'a = `modrm (R64PlusR64 (base, ofs))
end

class mem_op_base base_reg = object
  method build : 'a. [> `modrm of modrm ] as 'a = `modrm (SingleR64 base_reg)
  method plus_reg (r: [> `r64 of r64 ]) = match r with 
    | `r64 r -> new mem_op_base_plus_reg (base_reg, r)
end

let (++) (op : mem_op_base) (r: [> `r64 of r64 ]) = op#plus_reg(r)

class mem_op = object
  method base reg = new mem_op_base reg
end

let qword_ptr = function
  | `r64 r -> ((new mem_op)#base(r))