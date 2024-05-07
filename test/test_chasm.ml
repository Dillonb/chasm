open Chasm
open Chasm__Chasm_types
open Chasm__Chasm_util
open Chasm__Chasm_exceptions
open Stdint
open Test_utils

let print_success = false

let int_to_hex = function
  | i when (is_int8 i) || (is_uint8 i) -> Printf.sprintf "%02X" (i land 0xFF)
  | i -> raise (Invalid_argument ("Integer " ^ (string_of_int i) ^ " is not a valid byte."))

let rec int_list_to_hex_string = function
  | [] -> ""
  | x :: [] -> int_to_hex x
  | x :: xs -> (int_to_hex x) ^ " " ^ int_list_to_hex_string(xs)

let registers_8 = [ al; cl; dl; bl; sil; dil; spl; bpl; r8b; r9b; r10b; r11b; r12b; r13b; r14b; r15b ]
let registers_16 = [ ax; cx; dx; bx; si; di; sp; bp; r8w; r9w; r10w; r11w; r12w; r13w; r14w; r15w ]
let registers_32 = [ eax; ecx; edx; ebx; esi; edi; esp; ebp; r8d; r9d; r10d; r11d; r12d; r13d; r14d; r15d ]
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

let rd_to_str = function
| `r32 Eax  -> "eax"
| `r32 Ecx  -> "ecx"
| `r32 Edx  -> "edx"
| `r32 Ebx  -> "ebx"
| `r32 Esi  -> "esi"
| `r32 Edi  -> "edi"
| `r32 Esp  -> "esp"
| `r32 Ebp  -> "ebp"
| `r32 R8d  -> "r8d"
| `r32 R9d  -> "r9d"
| `r32 R10d -> "r10d"
| `r32 R11d -> "r11d"
| `r32 R12d -> "r12d"
| `r32 R13d -> "r13d"
| `r32 R14d -> "r14d"
| `r32 R15d -> "r15d"
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

let rb_to_str = function 
| `r8 Al   -> "al"
| `r8 Cl   -> "cl"
| `r8 Dl   -> "dl"
| `r8 Bl   -> "bl"
| `r8 Sil  -> "sil"
| `r8 Dil  -> "dil"
| `r8 Spl  -> "spl"
| `r8 Bpl  -> "bpl"
| `r8 R8b  -> "r8b"
| `r8 R9b  -> "r9b"
| `r8 R10b -> "r10b"
| `r8 R11b -> "r11b"
| `r8 R12b -> "r12b"
| `r8 R13b -> "r13b"
| `r8 R14b -> "r14b"
| `r8 R15b -> "r15b"
| _ -> raise (Invalid_argument "Unknown register passed!")

let r_to_str = function
  | `r64 r -> rq_to_str (`r64 r)
  | `r32 r -> rd_to_str (`r32 r)
  | `r16 r -> rw_to_str (`r16 r)
  | `r8  r -> rb_to_str (`r8 r)

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
  (push (imm16_i (-1))), "push 0xffff";
  (push (imm16_i 0x1234)), "push 0x1234";
  (push (imm16_i 0x1000)), "push 0x1000";
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

let temp_sub_testcases = [
  sub al   (uimm8_i 1), "sub al, 1";
  sub cl   (uimm8_i 1), "sub cl, 1";
  sub dl   (uimm8_i 1), "sub dl, 1";
  sub bl   (uimm8_i 1), "sub bl, 1";
  sub sil  (uimm8_i 1), "sub sil, 1";
  sub dil  (uimm8_i 1), "sub dil, 1";
  sub spl  (uimm8_i 1), "sub spl, 1";
  sub bpl  (uimm8_i 1), "sub bpl, 1";
  sub r8b  (uimm8_i 1), "sub r8b, 1";
  sub r9b  (uimm8_i 1), "sub r9b, 1";
  sub r10b (uimm8_i 1), "sub r10b, 1";
  sub r11b (uimm8_i 1), "sub r11b, 1";
  sub r12b (uimm8_i 1), "sub r12b, 1";
  sub r13b (uimm8_i 1), "sub r13b, 1";
  sub r14b (uimm8_i 1), "sub r14b, 1";
  sub r15b (uimm8_i 1), "sub r15b, 1";

  sub al (imm 1),      "sub al, 1";
  sub ax (uimm16_i 1),  "sub ax, 1";
  sub eax (uimm32_i 1), "sub eax, 1";
  sub rax (imm32_i 1), "sub rax, 1";
]

let map_all_combinations f l =
  List.concat_map (fun outer_elem -> List.map (fun inner_elem -> (f outer_elem inner_elem)) l) l

let scale_values = [-1; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; ]
let offset_values = [-1; -5; -9; -10; 0; 1; 5; 9; 10; -127; -128; -65536; 65536; -2147483648; 2147483647 ]

let string_of_offset = function
  | ofs when ofs = 0 -> ""
  | ofs when ofs < 10 && ofs > 0 -> Printf.sprintf " + %d" ofs
  | ofs when ofs > -10 && ofs < 0 -> Printf.sprintf " - %d" (abs ofs)
  | ofs when ofs < 0 -> Printf.sprintf " - 0x%x" (abs ofs)
  | ofs -> Printf.sprintf " + 0x%x" ofs

let is_valid_scale scale = scale == 1 || scale == 2 || scale == 4 || scale == 8

let mem_size_16_64_to_ptr_str = function
  | M16 -> "word ptr"
  | M64 -> "qword ptr"

let push_r64_testcases = List.map (fun reg -> (push reg), "push " ^ (rq_to_str reg)) registers_64
let push_r16_testcases = List.map (fun reg -> (push reg), "push " ^ (rw_to_str reg)) registers_16

let indirect_reg_testcases instr instr_name ptr_size regs =
  let ptr_func, regs = match ptr_size, regs with
    | M16, `r64 regs -> word_ptr_of_reg,  regs
    | M64, `r64 regs -> qword_ptr_of_reg, regs
    | M16, `r32 regs -> word_ptr_of_reg,  regs
    | M64, `r32 regs -> qword_ptr_of_reg, regs in
    List.map (fun reg -> (instr (ptr_func reg)), instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ (r_to_str reg) ^ "]") regs

let indirect_reg_m64_testcases instr instr_name regs =
  let ptr_func, regs = match regs with
    | `r64 regs -> qword_ptr_of_reg, regs
    | `r32 regs -> qword_ptr_of_reg, regs in
    List.map (fun reg -> (instr (ptr_func reg)), instr_name ^ " qword ptr [" ^ (r_to_str reg) ^ "]") regs

let indirect_reg_plus_reg_testcases instr instr_name ptr_size regs =
  let ptr_func, regs, sp = match ptr_size, regs with
    | M16, `r32 regs -> word_ptr_of_reg_plus_reg, regs, esp
    | M64, `r32 regs -> qword_ptr_of_reg_plus_reg, regs, esp
    | M16, `r64 regs -> word_ptr_of_reg_plus_reg, regs, rsp
    | M64, `r64 regs -> qword_ptr_of_reg_plus_reg, regs, rsp in
  map_all_combinations (fun reg1 reg2 -> let ins = (instr (ptr_func reg1 reg2)) in
   match reg1, reg2 with
    (* sp is invalid in the index field, so expect the assembler to quietly swap r1 and r2*)
    | base, index when base <> sp && index = sp -> ins, instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ r_to_str(sp) ^" + " ^ (r_to_str base) ^ "]"
    (* when rsp is in both fields, nothing we can do to fix it, so expect it to fail *)
    | base, index when base = sp && index = sp -> ins, instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ r_to_str(sp) ^" + " ^ r_to_str(sp) ^"] *INVALID*"
    (* normal case*)
    | reg1, reg2 -> ins, instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ (r_to_str reg1) ^ " + " ^ (r_to_str reg2) ^ "]")
    regs

let indirect_reg_scaled_testcases instr instr_name ptr_size regs =
  let ptr_func, regs, sp = match ptr_size, regs with
    | M16, `r32 regs -> word_ptr_of_reg_scaled, regs, esp
    | M64, `r32 regs -> qword_ptr_of_reg_scaled, regs, esp
    | M16, `r64 regs -> word_ptr_of_reg_scaled, regs, rsp
    | M64, `r64 regs -> qword_ptr_of_reg_scaled, regs, rsp in
  List.concat_map (fun scale ->
    List.map (fun reg -> match reg with
    | r when (r == sp && scale <> 1) || (not (is_valid_scale scale)) -> (instr (ptr_func reg scale)), instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ (r_to_str reg) ^ "*" ^ string_of_int scale ^ "] *INVALID*"
    | _ when scale == 1 -> (instr (ptr_func reg scale)), instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ (r_to_str reg) ^ "]"
    | _ -> (instr (ptr_func reg scale)), instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ (r_to_str reg) ^ "*" ^ string_of_int scale ^ "]"
    ) regs
  ) scale_values

let indirect_reg_plus_reg_times_scale_testcases instr instr_name ptr_size regs =
  let ptr_func, regs, sp = match ptr_size, regs with
    | M16, `r32 regs -> word_ptr_of_reg_plus_reg_scaled, regs, esp
    | M64, `r32 regs -> qword_ptr_of_reg_plus_reg_scaled, regs, esp
    | M16, `r64 regs -> word_ptr_of_reg_plus_reg_scaled, regs, rsp
    | M64, `r64 regs -> qword_ptr_of_reg_plus_reg_scaled, regs, rsp in
    List.concat_map (
      fun scale -> map_all_combinations (
        fun base index ->
          let ins = (instr (ptr_func base index scale)) in
            match base, index, scale with
              (* sp is invalid in the index field, so expect the assembler to quietly swap r1 and r2*)
              | base, index, scale when base <> sp && index = sp && scale = 1 -> ins, instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ r_to_str sp ^ " + " ^ (r_to_str base) ^ "]"
              (* when sp is in both fields, nothing we can do to fix it, so expect it to fail *)
              | base, index, scale when base = sp && index = sp -> ins, instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ r_to_str sp ^ " + " ^ r_to_str sp ^ "*" ^ (string_of_int scale) ^"] *INVALID*"
              (* when sp is in the index field and a scaling factor is used, nothing we can do to fix it, so expect it to fail *)
              | base, index, scale when index = sp && scale <> 1 -> ins, instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ (r_to_str base) ^ " + " ^ r_to_str sp ^ "*" ^ (string_of_int scale) ^"] *INVALID*"

              (* invalid test invalid scale values *)
              | base, index, scale when not (is_valid_scale scale) -> ins, instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ (r_to_str base) ^ " + " ^ (r_to_str index) ^ "*" ^ (string_of_int scale) ^"] *INVALID*"

              | base, index, scale when scale = 1 -> ins, instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ (r_to_str base) ^ " + " ^ (r_to_str index) ^ "]"
              | base, index, scale -> ins, instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ (r_to_str base) ^ " + " ^ (r_to_str index) ^ "*" ^ (string_of_int scale) ^"]"
        ) regs) scale_values

let indirect_reg_plus_offset_testcases instr instr_name ptr_size regs =
  let ptr_func, regs = match ptr_size, regs with
    | M16, `r32 regs -> word_ptr_of_reg_plus_offset, regs
    | M64, `r32 regs -> qword_ptr_of_reg_plus_offset, regs
    | M16, `r64 regs -> word_ptr_of_reg_plus_offset, regs
    | M64, `r64 regs -> qword_ptr_of_reg_plus_offset, regs in
  List.concat_map (fun offset ->
    List.map (fun reg -> (instr (ptr_func reg offset)), instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ (r_to_str reg) ^ string_of_offset offset ^ "]") regs
  ) offset_values

let indirect_r64_plus_r64_plus_offset_testcases instr instr_name ptr_size regs =
  let ptr_func, regs, sp = match ptr_size, regs with
    | M16, `r32 regs -> word_ptr_of_reg_plus_reg_plus_offset, regs, esp
    | M64, `r32 regs -> qword_ptr_of_reg_plus_reg_plus_offset, regs, esp
    | M16, `r64 regs -> word_ptr_of_reg_plus_reg_plus_offset, regs, rsp
    | M64, `r64 regs -> qword_ptr_of_reg_plus_reg_plus_offset, regs, rsp in
  List.concat_map (fun offset ->
    map_all_combinations (fun reg1 reg2 -> let ins = (instr (ptr_func reg1 reg2 offset)) in
    match reg1, reg2 with
      (* sp is invalid in the index field, so expect the assembler to quietly swap r1 and r2*)
      | base, index when base <> sp && index = sp -> ins, instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ r_to_str sp ^ " + " ^ (r_to_str base) ^ string_of_offset offset ^ "]"
      (* when sp is in both fields, nothing we can do to fix it, so expect it to fail *)
      | base, index when base = sp && index = sp -> ins, instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ r_to_str sp ^ " + " ^ r_to_str sp ^ "" ^ string_of_offset offset ^ "] *INVALID*"
      (* normal case*)
      | reg1, reg2 -> ins, instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ (r_to_str reg1) ^ " + " ^ (r_to_str reg2) ^ string_of_offset offset ^ "]")
      regs
  ) offset_values

let indirect_reg_plus_reg_times_scale_plus_offset_testcases instr instr_name ptr_size regs =
  let ptr_func, regs, sp = match ptr_size, regs with
    | M16, `r32 regs -> word_ptr_of_reg_plus_reg_scaled_plus_offset, regs, esp
    | M64, `r32 regs -> qword_ptr_of_reg_plus_reg_scaled_plus_offset, regs, esp
    | M16, `r64 regs -> word_ptr_of_reg_plus_reg_scaled_plus_offset, regs, rsp
    | M64, `r64 regs -> qword_ptr_of_reg_plus_reg_scaled_plus_offset, regs, rsp in
  List.concat_map (fun offset -> List.concat_map (
    fun scale -> map_all_combinations (
      fun base index ->
        let ins = (instr (ptr_func base index scale offset)) in
          match base, index, scale with
            (* sp is invalid in the index field, so expect the assembler to quietly swap r1 and r2*)
            | base, index, scale when base <> sp && index = sp && scale = 1 -> ins, instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ r_to_str sp ^ " + " ^ (r_to_str base) ^ string_of_offset offset ^ "]"
            (* when sp is in both fields, nothing we can do to fix it, so expect it to fail *)
            | base, index, scale when base = sp && index = sp -> ins, instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ r_to_str sp ^ " + " ^ r_to_str sp ^ "*" ^ (string_of_int scale) ^ string_of_offset offset ^"] *INVALID*"
            (* when sp is in the index field and a scaling factor is used, nothing we can do to fix it, so expect it to fail *)
            | base, index, scale when index = sp && scale <> 1 -> ins, instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ (r_to_str base) ^ " + " ^ r_to_str sp ^ "*" ^ (string_of_int scale) ^ string_of_offset offset ^"] *INVALID*"

            (* invalid test invalid scale values *)
            | base, index, scale when not (is_valid_scale scale) -> ins, instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ (r_to_str base) ^ " + " ^ (r_to_str index) ^ "*" ^ (string_of_int scale) ^ string_of_offset offset ^"] *INVALID*"

            | base, index, scale when scale = 1 -> ins, instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ (r_to_str base) ^ " + " ^ (r_to_str index) ^ string_of_offset offset ^ "]"
            | base, index, scale -> ins, instr_name ^ " " ^ mem_size_16_64_to_ptr_str ptr_size ^ " [" ^ (r_to_str base) ^ " + " ^ (r_to_str index) ^ "*" ^ (string_of_int scale) ^ string_of_offset offset ^"]"
      ) regs) scale_values) offset_values

let binary_r8_uimm8_testcases instr instr_name =
  List.concat_map (
    fun reg -> [
      (instr reg (uimm8_i 1)), instr_name ^ " " ^ (rb_to_str reg) ^ ", 1";
      (instr reg (uimm8_i 0xff)), instr_name ^ " " ^ (rb_to_str reg) ^ ", 0xff";
    ]
  ) registers_8

let num_failures = ref 0
let num_success = ref 0
let inc_failures () = num_failures := !num_failures + 1
let inc_success () = num_success := !num_success + 1
let safe_assemble instruction =
  let do_assemble instruction =
    let b = new chasm_block in
      b#append_instruction instruction; b#as_int_list in
  try Ok(do_assemble instruction) with
  | ex -> Error ex
let safe_disassemble asm =
  try Ok(Capstone.disassemble asm) with
  | ex -> Error ex

let rec validate_testcases = function
  | [] -> ()
  | (instruction, expected_mnemonic) :: remaining ->
    let _ = match safe_assemble instruction with
    | Ok asm -> (
      match safe_disassemble (make_bytes asm) with
      | Ok(disassembly) when (disassembly = expected_mnemonic) -> inc_success (); if print_success then Printf.printf "%s OK!\t%s\n" expected_mnemonic (int_list_to_hex_string asm) else ()
      | Ok(disassembly) -> print_endline (Printf.sprintf "%s FAILED! Assembled to %s - which disassembles to %s" expected_mnemonic (int_list_to_hex_string asm) disassembly); inc_failures ()
      | Error ex -> Printf.printf "%s assembled to %s - Failed to disassemble with exception: %s\n" expected_mnemonic (int_list_to_hex_string asm) (Printexc.to_string ex); inc_failures ()
    )
    | Error (Invalid_encoding _) when String.ends_with ~suffix:"*INVALID*" expected_mnemonic -> inc_success (); if print_success then Printf.printf "%s OK!\n" expected_mnemonic
    | Error ex -> Printf.printf "%s Failed to assemble with exception: %s\n" expected_mnemonic (Printexc.to_string ex); inc_failures ()
    in validate_testcases remaining

let () =
validate_testcases push_imm_testcases;
validate_testcases push_r16_testcases;
validate_testcases push_r64_testcases;

validate_testcases (indirect_reg_testcases push "push" M16 (`r64 registers_64));
validate_testcases (indirect_reg_testcases push "push" M64 (`r64 registers_64));
validate_testcases (indirect_reg_testcases push "push" M16 (`r32 registers_32));
validate_testcases (indirect_reg_testcases push "push" M64 (`r32 registers_32));

validate_testcases (indirect_reg_m64_testcases jmp "jmp" (`r32 registers_32));
validate_testcases (indirect_reg_m64_testcases jmp "jmp" (`r64 registers_32));

validate_testcases (indirect_reg_plus_reg_testcases push "push" M16 (`r64 registers_64));
validate_testcases (indirect_reg_plus_reg_testcases push "push" M64 (`r64 registers_64));
validate_testcases (indirect_reg_plus_reg_testcases push "push" M16 (`r32 registers_32));
validate_testcases (indirect_reg_plus_reg_testcases push "push" M64 (`r32 registers_32));

validate_testcases (indirect_reg_scaled_testcases push "push" M16 (`r64 registers_64));
validate_testcases (indirect_reg_scaled_testcases push "push" M64 (`r64 registers_64));
validate_testcases (indirect_reg_scaled_testcases push "push" M16 (`r32 registers_32));
validate_testcases (indirect_reg_scaled_testcases push "push" M64 (`r32 registers_32));

validate_testcases (indirect_reg_plus_reg_times_scale_testcases push "push" M16 (`r64 registers_64));
validate_testcases (indirect_reg_plus_reg_times_scale_testcases push "push" M64 (`r64 registers_64));
validate_testcases (indirect_reg_plus_reg_times_scale_testcases push "push" M16 (`r32 registers_32));
validate_testcases (indirect_reg_plus_reg_times_scale_testcases push "push" M64 (`r32 registers_32));

validate_testcases (indirect_reg_plus_offset_testcases push "push" M16 (`r64 registers_64));
validate_testcases (indirect_reg_plus_offset_testcases push "push" M64 (`r64 registers_64));
validate_testcases (indirect_reg_plus_offset_testcases push "push" M16 (`r32 registers_32));
validate_testcases (indirect_reg_plus_offset_testcases push "push" M64 (`r32 registers_32));

validate_testcases (indirect_r64_plus_r64_plus_offset_testcases push "push" M16 (`r64 registers_64));
validate_testcases (indirect_r64_plus_r64_plus_offset_testcases push "push" M64 (`r64 registers_64));
validate_testcases (indirect_r64_plus_r64_plus_offset_testcases push "push" M16 (`r32 registers_32));
validate_testcases (indirect_r64_plus_r64_plus_offset_testcases push "push" M64 (`r32 registers_32));

validate_testcases (indirect_reg_plus_reg_times_scale_plus_offset_testcases push "push" M16 (`r64 registers_64));
validate_testcases (indirect_reg_plus_reg_times_scale_plus_offset_testcases push "push" M64 (`r64 registers_64));
validate_testcases (indirect_reg_plus_reg_times_scale_plus_offset_testcases push "push" M16 (`r32 registers_32));
validate_testcases (indirect_reg_plus_reg_times_scale_plus_offset_testcases push "push" M64 (`r32 registers_32));

validate_testcases (binary_r8_uimm8_testcases sub "sub");

validate_testcases temp_sub_testcases;

Printf.printf "Passed %d testcases!\n" !num_success;
if (!num_failures > 0) then raise (Failure (Printf.sprintf "Failed %d test case%s!" !num_failures (if !num_failures == 1 then "" else "s"))) else ()

let () =
let expected = "0000000000000000 push rax\n\
0000000000000001 push qword ptr [eax]\n\
0000000000000004 jmp 1\n\
0000000000000006 push qword ptr [eax]\n\
0000000000000009 jmp 1\n\
000000000000000E jmp 0x15\n\
0000000000000010 jmp 0x15\n\
0000000000000015 push 0xabcdef\n" in
  let b = new chasm_block in
    b#push rax;
    b#label "back_label";
    b#push (qword_ptr_of_r32 eax);
    b#jmp (to_label "back_label");
    b#push (qword_ptr_of_r32 eax);
    b#jmp (to_label_long "back_label");
    b#jmp (to_label "forward_label");
    b#jmp (to_label_long "forward_label");
    b#label "forward_label";
    b#push (imm32_i 0xABCDEF);
    let actual = (Capstone.disassemble_all b#as_bytes) in
      if (actual <> expected) then (print_endline ("Actual:\n" ^ actual ^ "\nExpected:\n" ^ expected); raise (Failure (Printf.sprintf "jmp to label test failed. Expected:\n[%s]\nActual:\n[%s]\n" expected actual))) else ()
(* let one = 1 *)
(* let two = 2 *)
  let () =
    let some_uint64 = (Uint64.of_string "0xFFFFFFFFFFFFFFFF") in
    let b = new chasm_block in
        b#append [%chasm {|
          push rbp
          push rbx
          push r12
          push r13
          push r14
          push r15
          sub rsp, 8
          mov64 r12, ${some_uint64}
        |}];

        print_endline (Capstone.disassemble_all b#as_bytes)