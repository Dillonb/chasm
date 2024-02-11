external capstone_disassemble: bytes -> int -> string = "disassemble"
let disassemble b = capstone_disassemble b (Bytes.length b)