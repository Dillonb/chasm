open Chasm_types
open Stdint


let rb_to_int = function
  | `r8 Al  -> 0 | `r8 Cl  -> 1 | `r8 Dl   -> 2  | `r8 Bl   -> 3  | `r8 Spl  -> 4  | `r8 Bpl  -> 5  | `r8 Sil  -> 6  | `r8 Dil  -> 7
  | `r8 R8b -> 8 | `r8 R9b -> 9 | `r8 R10b -> 10 | `r8 R11b -> 11 | `r8 R12b -> 12 | `r8 R13b -> 13 | `r8 R14b -> 14 | `r8 R15b -> 15


let rw_to_int = function
  | `r16 Ax ->  0  | `r16 Cx ->  1  | `r16 Dx ->  2   | `r16 Bx ->  3   | `r16 Sp ->  4   | `r16 Bp ->  5   | `r16 Si ->  6 | `r16 Di ->  7
  | `r16 R8w ->  8 | `r16 R9w ->  9 | `r16 R10w -> 10 | `r16 R11w -> 11 | `r16 R12w -> 12 | `r16 R13w -> 13 | `r16 R14w -> 14 | `r16 R15w -> 15


let rd_to_int = function
  | `r32 Eax ->  0 | `r32 Ecx ->  1 | `r32 Edx  ->  2 | `r32 Ebx ->  3 | `r32 Esp   ->  4 | `r32 Ebp  ->  5 | `r32 Esi  ->  6 | `r32 Edi  ->  7
  | `r32 R8d ->  8 | `r32 R9d ->  9 | `r32 R10d -> 10 | `r32 R11d -> 11 | `r32 R12d -> 12 | `r32 R13d -> 13 | `r32 R14d -> 14 | `r32 R15d -> 15


let rq_to_int = function
  | `r64 Rax ->  0 | `r64 Rcx ->  1 | `r64 Rdx ->  2 | `r64 Rbx ->  3 | `r64 Rsp ->  4 | `r64 Rbp ->  5 | `r64 Rsi ->  6 | `r64 Rdi ->  7
  | `r64 R8  ->  8 | `r64 R9  ->  9 | `r64 R10 -> 10 | `r64 R11 -> 11 | `r64 R12 -> 12 | `r64 R13 -> 13 | `r64 R14 -> 14 | `r64 R15 -> 15

let is_int8 x =
  let min = Int8.to_int Int8.min_int in
  let max = Int8.to_int Int8.max_int in
    ((x >= min) && (x <= max))

let is_int16 x =
  let min = Int16.to_int Int16.min_int in
  let max = Int16.to_int Int16.max_int in
    ((x >= min) && (x <= max))
let is_int32 x =
  let min = Int32.to_int Int32.min_int in
  let max = Int32.to_int Int32.max_int in
    ((x >= min) && (x <= max))

let int_to_sized_imm = function
  | x when is_int8 x  -> `imm8 (Int8.of_int x)
  | x when is_int16 x -> `imm16 (Int16.of_int x)
  | x when is_int32 x -> `imm32 (Int32.of_int x)
  | _ -> raise (Invalid_argument "Int either too large or too small to represent in an immediate!")


let list_of_int16_le x = let x = Int16.to_int x in [ x land 0xFF; (x lsr 8) land 0xFF; ]
let list_of_int32_le x = let x = Int32.to_int x in [ x land 0xFF; (x lsr 8) land 0xFF; (x lsr 16) land 0xFF; (x lsr 24) land 0xFF ]

let int_of_bool b = if b then 1 else 0
let make_rex w r x b = let low_bits =
    ((int_of_bool w) lsl 3) lor
    ((int_of_bool r) lsl 2) lor
    (int_of_bool x lsl 1) lor
    (int_of_bool b) in
  if (low_bits = 0) then Some (0x40 lor low_bits) else None

let make_modrm modbits reg rm =
  ((modbits land 0b11) lsl 6) lor
  ((reg land 0b111) lsl 3) lor
  (rm land 0b111)

let make_sib scale index base =
  ((scale land 0b11) lsl 6) lor
  ((index land 0b111) lsl 3) lor
  (base land 0b111)

let rex_b = make_rex false false false true

let make_bytes l = let b = Bytes.create (List.length l) in
  List.iteri (fun i v -> Bytes.set_uint8 b i v) l; b


(** prepends an optional to a list *)
let (+?) o l = match o with
  | Some i -> i :: l
  | None -> l

(** appends an optional to a list *)
let (@?) l o = match o with
  | Some i -> l @ [i]
  | None -> l