external capstone_disassemble: bytes -> int -> string = "disassemble"
external capstone_disassemble_all: bytes -> int -> string = "disassemble_all"
let disassemble b = capstone_disassemble b (Bytes.length b)
let disassemble_all b = capstone_disassemble_all b (Bytes.length b)