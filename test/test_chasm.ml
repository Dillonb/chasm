open Chasm
open Chasm__Chasm_types
open Stdint

let bytes_to_hex_string bytes = 
  let fold_fn c s = Printf.sprintf "%02X" (int_of_char c) :: s in
    String.concat " " (Bytes.fold_right fold_fn bytes [])

let registers_16 = [ ax; cx; dx; bx; si; di; sp; bp; r8w; r9w; r10w; r11w; r12w; r13w; r14w; r15w ]
let registers_64 = [ rax; rcx; rdx; rbx; rsi; rdi; rsp; rbp; r8; r9; r10; r11; r12; r13; r14; r15 ]
let rq_to_str = function
| `r64 Rax -> "rax"
| `r64 Rcx -> "rcx"
| `r64 Rdx -> "rdx"
| `r64 Rbx -> "rbx"
| `r64 Rsi -> "rsi"
| `r64 Rdi -> "rdi"
| `r64 Rsp -> "rsp"
| `r64 Rbp -> "rbp"
| `r64 R8  -> "r8"
| `r64 R9  -> "r9"
| `r64 R10 -> "r10"
| `r64 R11 -> "r11"
| `r64 R12 -> "r12"
| `r64 R13 -> "r13"
| `r64 R14 -> "r14"
| `r64 R15 -> "r15"
| _ -> raise (Invalid_argument "Unknown register passed!")

let rw_to_str = function
| `r16 Ax -> "ax"
| `r16 Cx -> "cx"
| `r16 Dx -> "dx"
| `r16 Bx -> "bx"
| `r16 Si -> "si"
| `r16 Di -> "di"
| `r16 Sp -> "sp"
| `r16 Bp -> "bp"
| `r16 R8w  -> "r8w"
| `r16 R9w  -> "r9w"
| `r16 R10w -> "r10w"
| `r16 R11w -> "r11w"
| `r16 R12w -> "r12w"
| `r16 R13w -> "r13w"
| `r16 R14w -> "r14w"
| `r16 R15w -> "r15w"
| _ -> raise (Invalid_argument "Unknown register passed!")

let push_imm_testcases = [
  (* push immediates *)
  (push (imm8 (Int8.of_int 0x12))), "push 0x12";
  (push (imm8 (Int8.of_int (-1)))), "push -1";
  (push (imm16 (Int16.of_int 0xFFFF))), "push 0xffff";
  (push (imm16 (Int16.of_int (-1)))), "push 0xffff";
  (push (imm16 (Int16.of_int 0x1234))), "push 0x1234";
  (push (imm16 (Int16.of_int 0x1000))), "push 0x1000";
  (push (imm32 (Int32.of_int 0xFFFFFFFF))), "push -1";
  (push (imm32 (Int32.of_int (-1)))), "push -1";
  (push (imm32 (Int32.of_int 0x12345678))), "push 0x12345678";
  (push (imm32 (Int32.of_int 0x10000000))), "push 0x10000000";
  (push (imm8 (Int8.of_int 12))), "push 0xc";
  
  (* same, but with _i helpers*)
  (push (imm8_i 0x12)), "push 0x12";
  (push (imm8_i (-1))), "push -1";
  (push (imm16_i 0xFFFF)), "push 0xffff";
  (push (imm16_i (-1))), "push 0xffff";
  (push (imm16_i 0x1234)), "push 0x1234";
  (push (imm16_i 0x1000)), "push 0x1000";
  (push (imm32_i 0xFFFFFFFF)), "push -1";
  (push (imm32_i (-1))), "push -1";
  (push (imm32_i 0x12345678)), "push 0x12345678";
  (push (imm32_i 0x10000000)), "push 0x10000000";
  (push (imm8_i 12)), "push 0xc";

  (* push immediates, detect the size *)
  (push (imm (-1))), "push -1";

  (push (imm (-128))), "push -0x80"; (* int8 min *)
  (push (imm (-129))), "push 0xff7f"; (* int8 min - 1, should assemble as an imm16 *)
  (push (imm 127)), "push 0x7f"; (* int8 max *)
  (push (imm 128)), "push 0x80"; (* int8 max + 1, should assemble as an imm16 *)

  (push (imm (-32768))), "push 0x8000"; (* int16 min *)
  (push (imm (-32769))), "push -0x8001"; (* int16 min - 1, should assemble as an imm32 *)
  (push (imm 32767)), "push 0x7fff"; (* int16 max *)
  (push (imm 32768)), "push 0x8000"; (* int16 max + 1, should assemble as an imm32 *)
]

let map_all_combinations f l =
  List.concat_map (fun outer_elem -> List.map (fun inner_elem -> (f outer_elem inner_elem)) l) l

let push_r64_testcases = List.map (fun reg -> (push reg), "push " ^ (rq_to_str reg)) registers_64
let push_r16_testcases = List.map (fun reg -> (push reg), "push " ^ (rw_to_str reg)) registers_16
let push_indirect_r64_testcases = List.map (fun reg -> (push (qword_ptr reg)#build), "push qword ptr [" ^ (rq_to_str reg) ^ "]") registers_64
let push_indirect_r64_plus_r64_testcases =
  map_all_combinations (fun reg1 reg2 -> 
      (push (qword_ptr reg1 ++ reg2)#build), 
      "push qword ptr [" ^ (rq_to_str reg1) ^ " + " ^ (rq_to_str reg2) ^ "]") 
    registers_64

let has_failure = ref false
let print_failure asm expected actual =
          print_endline (Printf.sprintf "%s FAILED! Assembled to %s - which disassembles to %s" expected (bytes_to_hex_string asm) actual);
          has_failure := true

let safe_assemble instruction =
  try Ok(assemble instruction) with
  | ex -> has_failure := true; Error ex
let safe_disassemble asm = 
  try Ok(Capstone.disassemble asm) with
  | ex -> has_failure := true; Error ex

let rec validate_testcases = function
  | [] -> ()
  | (instruction, expected_mnemonic) :: remaining ->
    Printf.printf "%s " expected_mnemonic;
    let _ = match safe_assemble instruction with
    | Ok asm -> (
      match safe_disassemble asm with
      | Ok(disassembly) when (disassembly = expected_mnemonic) -> print_endline (Printf.sprintf "OK!\t%s" (bytes_to_hex_string asm))
      | Ok(disassembly) -> print_failure asm expected_mnemonic disassembly
      | Error ex -> Printf.printf "Failed to disassemble with exception: %s\n" (Printexc.to_string ex)
    )
    | Error ex -> Printf.printf "Failed to assemble with exception: %s\n" (Printexc.to_string ex)
    in validate_testcases remaining

let () = 
validate_testcases push_imm_testcases;
validate_testcases push_r16_testcases;
validate_testcases push_r64_testcases;
validate_testcases push_indirect_r64_testcases;
validate_testcases push_indirect_r64_plus_r64_testcases;

if (!has_failure) then raise (Failure "Failed one or more test cases!") else ()