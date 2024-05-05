{
open Lexing
open X64_parser

exception SyntaxError of string
}

let dec_digit = ['0'-'9']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let letter = ['a'-'z' 'A'-'Z']

let int = '-'? dec_digit dec_digit*
let hex = "0x" hex_digit hex_digit*

let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
(* let anything = [' ' 'a'-'z' 'A'-'Z' '0'-'9' '!' '@' '#' '$' '%' '^' '&' '*' '(' ')' '_' '-' '<' '>' '?' ',' '.' '/' ':' ';' '[' ']']* *)
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
    parse
    | whitespace { read lexbuf }
    | newline    { new_line lexbuf; read lexbuf }
    | int        { INT (int_of_string (lexeme lexbuf)) }
    | hex        { INT (int_of_string (lexeme lexbuf)) }
    | eof        { EOF }

    | '[' { OPEN_BRACKET }
    | ']' { CLOSE_BRACKET }
    | '(' { OPEN_PAREN }
    | ')' { CLOSE_PAREN }
    | '{' { OPEN_BRACE }
    | '}' { CLOSE_BRACE }
    | ',' { COMMA }
    | ':' { COLON }
    | '+' { PLUS }
    | '*' { MULT }
    | '$' { DOLLAR_SIGN }

    | "long_label" { LONG_LABEL }

    | "rb" { RB }
    | "rw" { RW }
    | "rd" { RD }
    | "rq" { RQ }

    | "byte"  { BYTE }
    | "word"  { WORD }
    | "dword" { DWORD }
    | "qword" { QWORD }
    | "ptr" { PTR }

    | "al"   { AL }
    | "cl"   { CL }
    | "dl"   { DL }
    | "bl"   { BL }
    | "sil"  { SIL }
    | "dil"  { DIL }
    | "spl"  { SPL }
    | "bpl"  { BPL }
    | "r8b"  { R8B }
    | "r9b"  { R9B }
    | "r10b" { R10B }
    | "r11b" { R11B }
    | "r12b" { R12B }
    | "r13b" { R13B }
    | "r14b" { R14B }
    | "r15b" { R15B }

    | "ax"   { AX }
    | "cx"   { CX }
    | "dx"   { DX }
    | "bx"   { BX }
    | "si"   { SI }
    | "di"   { DI }
    | "sp"   { SP }
    | "bp"   { BP }
    | "r8w"  { R8W }
    | "r9w"  { R9W }
    | "r10w" { R10W }
    | "r11w" { R11W }
    | "r12w" { R12W }
    | "r13w" { R13W }
    | "r14w" { R14W }
    | "r15w" { R15W }

    | "eax"  { EAX }
    | "ecx"  { ECX }
    | "edx"  { EDX }
    | "ebx"  { EBX }
    | "esi"  { ESI }
    | "edi"  { EDI }
    | "esp"  { ESP }
    | "ebp"  { EBP }
    | "r8d"  { R8D }
    | "r9d"  { R9D }
    | "r10d" { R10D }
    | "r11d" { R11D }
    | "r12d" { R12D }
    | "r13d" { R13D }
    | "r14d" { R14D }
    | "r15d" { R15D }

    | "rax" { RAX }
    | "rcx" { RCX }
    | "rdx" { RDX }
    | "rbx" { RBX }
    | "rsi" { RSI }
    | "rdi" { RDI }
    | "rsp" { RSP }
    | "rbp" { RBP }
    | "r8"  { R8 }
    | "r9"  { R9 }
    | "r10" { R10 }
    | "r11" { R11 }
    | "r12" { R12 }
    | "r13" { R13 }
    | "r14" { R14 }
    | "r15" { R15 }

    | "push" { PUSH }
    | "sub"  { SUB }
    | "jmp"  { JMP }

    | identifier { IDENTIFIER (lexeme lexbuf) }
    (* | anything { OCAML_CODE (lexeme lexbuf) } *)
    (* don't put anything after this!
       needs to be last so all other identifiers take precedence *)