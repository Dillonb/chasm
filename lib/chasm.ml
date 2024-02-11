open Chasm_types

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
  | 0 -> al
  | 1 -> cl
  | 2 -> dl
  | 3 -> bl
  | 4 -> spl
  | 5 -> bpl
  | 6 -> sil
  | 7 -> dil
  | 8 -> r8b
  | 9 -> r9b
  | 10 -> r10b
  | 11 -> r11b
  | 12 -> r12b
  | 13 -> r13b
  | 14 -> r14b
  | 15 -> r15b
  | x -> raise (Invalid_argument (Printf.sprintf "Invalid register: %d valid values are 0-15" x))

let rw = function
  | 0 ->  ax
  | 1 ->  cx
  | 2 ->  dx
  | 3 ->  bx
  | 4 ->  sp
  | 5 ->  bp
  | 6 ->  si
  | 7 ->  di
  | 8 ->  r8w
  | 9 ->  r9w
  | 10 -> r10w
  | 11 -> r11w
  | 12 -> r12w
  | 13 -> r13w
  | 14 -> r14w
  | 15 -> r15w
  | x -> raise (Invalid_argument (Printf.sprintf "Invalid register: %d valid values are 0-15" x))

let rd = function
  | 0 ->  eax
  | 1 ->  ecx
  | 2 ->  edx
  | 3 ->  ebx
  | 4 ->  esp
  | 5 ->  ebp
  | 6 ->  esi
  | 7 ->  edi
  | 8 ->  r8d
  | 9 ->  r9d
  | 10 -> r10d
  | 11 -> r11d
  | 12 -> r12d
  | 13 -> r13d
  | 14 -> r14d
  | 15 -> r15d
  | x -> raise (Invalid_argument (Printf.sprintf "Invalid register: %d valid values are 0-15" x))

let rq = function
  | 0 ->  rax
  | 1 ->  rcx
  | 2 ->  rdx
  | 3 ->  rbx
  | 4 ->  rsp
  | 5 ->  rbp
  | 6 ->  rsi
  | 7 ->  rdi
  | 8 ->  r8
  | 9 ->  r9
  | 10 -> r10
  | 11 -> r11
  | 12 -> r12
  | 13 -> r13
  | 14 -> r14
  | 15 -> r15
  | x -> raise (Invalid_argument (Printf.sprintf "Invalid register: %d valid values are 0-15" x))

(*
let rb_to_int = function
  | `r8 Al -> 0
  | `r8 Cl -> 1
  | `r8 Dl -> 2
  | `r8 Bl -> 3
  | `r8 Spl -> 4
  | `r8 Bpl -> 5
  | `r8 Sil -> 6
  | `r8 Dil -> 7
  | `r8 R8b -> 8
  | `r8 R9b -> 9
  | `r8 R10b -> 10
  | `r8 R11b -> 11
  | `r8 R12b -> 12
  | `r8 R13b -> 13
  | `r8 R14b -> 14
  | `r8 R15b -> 15
  *)

let rw_to_int = function
  | `r16 Ax ->  0
  | `r16 Cx ->  1
  | `r16 Dx ->  2
  | `r16 Bx ->  3
  | `r16 Sp ->  4
  | `r16 Bp ->  5
  | `r16 Si ->  6
  | `r16 Di ->  7
  | `r16 R8w ->  8
  | `r16 R9w ->  9
  | `r16 R10w -> 10
  | `r16 R11w -> 11
  | `r16 R12w -> 12
  | `r16 R13w -> 13
  | `r16 R14w -> 14
  | `r16 R15w -> 15

  (*
let rd_to_int = function
  | `r32 Eax ->  0
  | `r32 Ecx ->  1
  | `r32 Edx ->  2
  | `r32 Ebx ->  3
  | `r32 Esp ->  4
  | `r32 Ebp ->  5
  | `r32 Esi ->  6
  | `r32 Edi ->  7
  | `r32 R8d ->  8
  | `r32 R9d ->  9
  | `r32 R10d -> 10
  | `r32 R11d -> 11
  | `r32 R12d -> 12
  | `r32 R13d -> 13
  | `r32 R14d -> 14
  | `r32 R15d -> 15
*)

let rq_to_int = function
  | `r64 Rax ->  0
  | `r64 Rcx ->  1
  | `r64 Rdx ->  2
  | `r64 Rbx ->  3
  | `r64 Rsp ->  4
  | `r64 Rbp ->  5
  | `r64 Rsi ->  6
  | `r64 Rdi ->  7
  | `r64 R8  ->  8
  | `r64 R9  ->  9
  | `r64 R10 -> 10
  | `r64 R11 -> 11
  | `r64 R12 -> 12
  | `r64 R13 -> 13
  | `r64 R14 -> 14
  | `r64 R15 -> 15


let push x = Push x

let make_bytes l = let b = Bytes.create (List.length l) in
  List.iteri (fun i v -> Bytes.set_uint8 b i v) l; b

let assemble = function
  | Push (`r16 r)  -> let reg_num = rw_to_int(`r16 r) in
                        if (reg_num < 8) then
                          make_bytes ([0x66; 0x50 + (rw_to_int (`r16 r))])
                        else
                          make_bytes [0x66; 0x41; 0x50 + (reg_num - 8)]
  | Push (`r64 r)  -> let reg_num = rq_to_int(`r64 r) in
                        if (reg_num < 8) then
                          make_bytes [0x50 + reg_num]
                        else
                          make_bytes [0x41; 0x50 + (reg_num - 8)]
  | Push (`mem _)  -> raise (Invalid_argument "todo: push mem")


let assemble_list instrs = let asm = List.map assemble instrs in
  Bytes.concat Bytes.empty asm