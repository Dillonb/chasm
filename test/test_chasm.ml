open Chasm

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
  (push (imm8 0x12)), "push 0x12";
  (push (imm8 (-1))), "push -1";
  (push (imm16 (0xFFFF))), "push 0xffff";
  (push (imm16 (0x1234))), "push 0x1234";
  (push (imm16 (0x1000))), "push 0x1000";
  (push (imm32 (0xFFFFFFFF))), "push -1";
  (push (imm32 (0x12345678))), "push 0x12345678";
  (push (imm32 (0x10000000))), "push 0x10000000";
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