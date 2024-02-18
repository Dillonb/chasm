open Chasm
open Stdint

let bytes_to_hex_string bytes = 
  let fold_fn c s = Printf.sprintf "%02X" (int_of_char c) :: s in
    String.concat " " (Bytes.fold_right fold_fn bytes [])

let push_testcases = [
  (* push 16 bit regs *)
  (push ax),   "push ax";
  (push cx),   "push cx";
  (push dx),   "push dx";
  (push bx),   "push bx";
  (push si),   "push si";
  (push di),   "push di";
  (push sp),   "push sp";
  (push bp),   "push bp";
  (push r8w),  "push r8w";
  (push r9w),  "push r9w";
  (push r10w), "push r10w";
  (push r11w), "push r11w";
  (push r12w), "push r12w";
  (push r13w), "push r13w";
  (push r14w), "push r14w";
  (push r15w), "push r15w";

  (* push 64 bit regs *)
  (push rax), "push rax";
  (push rcx), "push rcx";
  (push rdx), "push rdx";
  (push rbx), "push rbx";
  (push rsi), "push rsi";
  (push rdi), "push rdi";
  (push rsp), "push rsp";
  (push rbp), "push rbp";
  (push r8),  "push r8";
  (push r9),  "push r9";
  (push r10), "push r10";
  (push r11), "push r11";
  (push r12), "push r12";
  (push r13), "push r13";
  (push r14), "push r14";
  (push r15), "push r15";

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

  (push (qword_ptr rax)#build),                "push qword [rax]";
  (push ((qword_ptr rax)#plus_reg rcx)#build), "push qword [rax + rcx]";
]

let print_failure asm expected actual =
          print_endline (Printf.sprintf "%s FAILED! Assembled to %s:" expected (bytes_to_hex_string asm));
          print_endline (Printf.sprintf "Which disassembles to: %s" actual);
          raise (Failure "Failed test case!")

let rec validate_testcases = function
  | [] -> ()
  | (instruction, expected_mnemonic) :: remaining ->
    let asm = assemble instruction in
      let disassembly = Capstone.disassemble asm in
        if (disassembly = expected_mnemonic) then
          print_endline (Printf.sprintf "%s OK!\t%s" expected_mnemonic (bytes_to_hex_string asm))
        else
          print_failure asm expected_mnemonic disassembly;
    validate_testcases remaining

let () = validate_testcases push_testcases