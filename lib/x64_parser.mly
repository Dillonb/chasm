%token <string> IDENTIFIER
%token <int> INT
%token <Stdint.uint64> INT64

%token OPEN_BRACKET
%token CLOSE_BRACKET
%token OPEN_PAREN
%token CLOSE_PAREN
%token OPEN_BRACE
%token CLOSE_BRACE
%token COMMA
%token COLON
%token PLUS
%token MULT
%token DOLLAR_SIGN
%token EOF

%token BYTE
%token WORD
%token DWORD
%token QWORD
%token PTR

%token LONG_LABEL

%token PUSH
%token SUB
%token JMP
%token ADD
%token MOV

%token AL
%token CL
%token DL
%token BL
%token SIL
%token DIL
%token SPL
%token BPL
%token R8B
%token R9B
%token R10B
%token R11B
%token R12B
%token R13B
%token R14B
%token R15B

%token RB
%token RW
%token RD
%token RQ

%token AX
%token CX
%token DX
%token BX
%token SI
%token DI
%token SP
%token BP
%token R8W
%token R9W
%token R10W
%token R11W
%token R12W
%token R13W
%token R14W
%token R15W

%token EAX
%token ECX
%token EDX
%token EBX
%token ESI
%token EDI
%token ESP
%token EBP
%token R8D
%token R9D
%token R10D
%token R11D
%token R12D
%token R13D
%token R14D
%token R15D

%token RAX
%token RCX
%token RDX
%token RBX
%token RSI
%token RDI
%token RSP
%token RBP
%token R8
%token R9
%token R10
%token R11
%token R12
%token R13
%token R14
%token R15

%start <Chasm__Chasm_types.asm_line list> program
%%

r8:
    | AL   {Chasm__Chasm_types.Al}
    | CL   {Chasm__Chasm_types.Cl}
    | DL   {Chasm__Chasm_types.Dl}
    | BL   {Chasm__Chasm_types.Bl}
    | SIL  {Chasm__Chasm_types.Sil}
    | DIL  {Chasm__Chasm_types.Dil}
    | SPL  {Chasm__Chasm_types.Spl}
    | BPL  {Chasm__Chasm_types.Bpl}
    | R8B  {Chasm__Chasm_types.R8b}
    | R9B  {Chasm__Chasm_types.R9b}
    | R10B {Chasm__Chasm_types.R10b}
    | R11B {Chasm__Chasm_types.R11b}
    | R12B {Chasm__Chasm_types.R12b}
    | R13B {Chasm__Chasm_types.R13b}
    | R14B {Chasm__Chasm_types.R14b}
    | R15B {Chasm__Chasm_types.R15b}

r16:
    | AX   {Chasm__Chasm_types.Ax}
    | CX   {Chasm__Chasm_types.Cx}
    | DX   {Chasm__Chasm_types.Dx}
    | BX   {Chasm__Chasm_types.Bx}
    | SI   {Chasm__Chasm_types.Si}
    | DI   {Chasm__Chasm_types.Di}
    | SP   {Chasm__Chasm_types.Sp}
    | BP   {Chasm__Chasm_types.Bp}
    | R8W  {Chasm__Chasm_types.R8w}
    | R9W  {Chasm__Chasm_types.R9w}
    | R10W {Chasm__Chasm_types.R10w}
    | R11W {Chasm__Chasm_types.R11w}
    | R12W {Chasm__Chasm_types.R12w}
    | R13W {Chasm__Chasm_types.R13w}
    | R14W {Chasm__Chasm_types.R14w}
    | R15W {Chasm__Chasm_types.R15w}

r32:
    | EAX  {Chasm__Chasm_types.Eax}
    | ECX  {Chasm__Chasm_types.Ecx}
    | EDX  {Chasm__Chasm_types.Edx}
    | EBX  {Chasm__Chasm_types.Ebx}
    | ESI  {Chasm__Chasm_types.Esi}
    | EDI  {Chasm__Chasm_types.Edi}
    | ESP  {Chasm__Chasm_types.Esp}
    | EBP  {Chasm__Chasm_types.Ebp}
    | R8D  {Chasm__Chasm_types.R8d}
    | R9D  {Chasm__Chasm_types.R9d}
    | R10D {Chasm__Chasm_types.R10d}
    | R11D {Chasm__Chasm_types.R11d}
    | R12D {Chasm__Chasm_types.R12d}
    | R13D {Chasm__Chasm_types.R13d}
    | R14D {Chasm__Chasm_types.R14d}
    | R15D {Chasm__Chasm_types.R15d}

r64:
    | RAX {Chasm__Chasm_types.Rax}
    | RCX {Chasm__Chasm_types.Rcx}
    | RDX {Chasm__Chasm_types.Rdx}
    | RBX {Chasm__Chasm_types.Rbx}
    | RSI {Chasm__Chasm_types.Rsi}
    | RDI {Chasm__Chasm_types.Rdi}
    | RSP {Chasm__Chasm_types.Rsp}
    | RBP {Chasm__Chasm_types.Rbp}
    | R8  {Chasm__Chasm_types.R8}
    | R9  {Chasm__Chasm_types.R9}
    | R10 {Chasm__Chasm_types.R10}
    | R11 {Chasm__Chasm_types.R11}
    | R12 {Chasm__Chasm_types.R12}
    | R13 {Chasm__Chasm_types.R13}
    | R14 {Chasm__Chasm_types.R14}
    | R15 {Chasm__Chasm_types.R15}
    | RQ; OPEN_PAREN; id = IDENTIFIER; CLOSE_PAREN {Chasm__Chasm_types.R64Runtime id}

byte_ptr:
    (* [reg] *)
    | BYTE PTR? OPEN_BRACKET; r = r64; CLOSE_BRACKET { Chasm.byte_ptr_of_r64 (`r64 r) }
    | BYTE PTR? OPEN_BRACKET; r = r32; CLOSE_BRACKET { Chasm.byte_ptr_of_r32 (`r32 r) }
    (* [reg + reg] *)
    | BYTE PTR? OPEN_BRACKET; base = r64; PLUS; index = r64; CLOSE_BRACKET { Chasm.byte_ptr_of_r64_plus_r64 (`r64 base) (`r64 index)}
    | BYTE PTR? OPEN_BRACKET; base = r32; PLUS; index = r32; CLOSE_BRACKET { Chasm.byte_ptr_of_r32_plus_r32 (`r32 base) (`r32 index)}
    (** [reg + reg * scale] *)
    | BYTE PTR? OPEN_BRACKET; base = r64; PLUS; index = r64; MULT; scale = INT; CLOSE_BRACKET { Chasm.byte_ptr_of_r64_plus_r64_scaled (`r64 base) (`r64 index) (scale)}
    | BYTE PTR? OPEN_BRACKET; base = r32; PLUS; index = r32; MULT; scale = INT; CLOSE_BRACKET { Chasm.byte_ptr_of_r32_plus_r32_scaled (`r32 base) (`r32 index) (scale)}
    (** [reg * scale] *)
    | BYTE PTR? OPEN_BRACKET; base = r64; MULT; scale = INT; CLOSE_BRACKET { Chasm.byte_ptr_of_r64_scaled (`r64 base) (scale)}
    | BYTE PTR? OPEN_BRACKET; base = r32; MULT; scale = INT; CLOSE_BRACKET { Chasm.byte_ptr_of_r32_scaled (`r32 base) (scale)}
    (** [reg + offset] *)
    | BYTE PTR? OPEN_BRACKET; base = r64; PLUS; offset = INT; CLOSE_BRACKET { Chasm.byte_ptr_of_r64_plus_offset (`r64 base) (offset)}
    | BYTE PTR? OPEN_BRACKET; base = r32; PLUS; offset = INT; CLOSE_BRACKET { Chasm.byte_ptr_of_r32_plus_offset (`r32 base) (offset)}
    (** [reg + reg + offset] *)
    | BYTE PTR? OPEN_BRACKET; base = r64; PLUS; index = r64; PLUS; offset = INT; CLOSE_BRACKET { Chasm.byte_ptr_of_r64_plus_r64_plus_offset (`r64 base) (`r64 index) (offset)}
    | BYTE PTR? OPEN_BRACKET; base = r32; PLUS; index = r32; PLUS; offset = INT; CLOSE_BRACKET { Chasm.byte_ptr_of_r32_plus_r32_plus_offset (`r32 base) (`r32 index) (offset)}
    (** [reg + reg * scale + offset] *)
    | BYTE PTR? OPEN_BRACKET; base = r64; PLUS; index = r64; MULT; scale = INT; PLUS; offset = INT; CLOSE_BRACKET { Chasm.byte_ptr_of_r64_plus_r64_scaled_plus_offset (`r64 base) (`r64 index) (scale) (offset)}
    | BYTE PTR? OPEN_BRACKET; base = r32; PLUS; index = r32; MULT; scale = INT; PLUS; offset = INT; CLOSE_BRACKET { Chasm.byte_ptr_of_r32_plus_r32_scaled_plus_offset (`r32 base) (`r32 index) (scale) (offset)}
    (** [reg * scale + offset] *)
    | BYTE PTR? OPEN_BRACKET; base = r64; MULT; scale = INT; PLUS; offset = INT; CLOSE_BRACKET { Chasm.byte_ptr_of_r64_scaled_plus_offset (`r64 base) (scale) (offset)}
    | BYTE PTR? OPEN_BRACKET; base = r32; MULT; scale = INT; PLUS; offset = INT; CLOSE_BRACKET { Chasm.byte_ptr_of_r32_scaled_plus_offset (`r32 base) (scale) (offset)}

word_ptr:
    (* [reg] *)
    | WORD PTR? OPEN_BRACKET; r = r64; CLOSE_BRACKET { Chasm.word_ptr_of_r64 (`r64 r) }
    | WORD PTR? OPEN_BRACKET; r = r32; CLOSE_BRACKET { Chasm.word_ptr_of_r32 (`r32 r) }
    (* [reg + reg] *)
    | WORD PTR? OPEN_BRACKET; base = r64; PLUS; index = r64; CLOSE_BRACKET { Chasm.word_ptr_of_r64_plus_r64 (`r64 base) (`r64 index)}
    | WORD PTR? OPEN_BRACKET; base = r32; PLUS; index = r32; CLOSE_BRACKET { Chasm.word_ptr_of_r32_plus_r32 (`r32 base) (`r32 index)}
    (** [reg + reg * scale] *)
    | WORD PTR? OPEN_BRACKET; base = r64; PLUS; index = r64; MULT; scale = INT; CLOSE_BRACKET { Chasm.word_ptr_of_r64_plus_r64_scaled (`r64 base) (`r64 index) (scale)}
    | WORD PTR? OPEN_BRACKET; base = r32; PLUS; index = r32; MULT; scale = INT; CLOSE_BRACKET { Chasm.word_ptr_of_r32_plus_r32_scaled (`r32 base) (`r32 index) (scale)}
    (** [reg * scale] *)
    | WORD PTR? OPEN_BRACKET; base = r64; MULT; scale = INT; CLOSE_BRACKET { Chasm.word_ptr_of_r64_scaled (`r64 base) (scale)}
    | WORD PTR? OPEN_BRACKET; base = r32; MULT; scale = INT; CLOSE_BRACKET { Chasm.word_ptr_of_r32_scaled (`r32 base) (scale)}
    (** [reg + offset] *)
    | WORD PTR? OPEN_BRACKET; base = r64; PLUS; offset = INT; CLOSE_BRACKET { Chasm.word_ptr_of_r64_plus_offset (`r64 base) (offset)}
    | WORD PTR? OPEN_BRACKET; base = r32; PLUS; offset = INT; CLOSE_BRACKET { Chasm.word_ptr_of_r32_plus_offset (`r32 base) (offset)}
    (** [reg + reg + offset] *)
    | WORD PTR? OPEN_BRACKET; base = r64; PLUS; index = r64; PLUS; offset = INT; CLOSE_BRACKET { Chasm.word_ptr_of_r64_plus_r64_plus_offset (`r64 base) (`r64 index) (offset)}
    | WORD PTR? OPEN_BRACKET; base = r32; PLUS; index = r32; PLUS; offset = INT; CLOSE_BRACKET { Chasm.word_ptr_of_r32_plus_r32_plus_offset (`r32 base) (`r32 index) (offset)}
    (** [reg + reg * scale + offset] *)
    | WORD PTR? OPEN_BRACKET; base = r64; PLUS; index = r64; MULT; scale = INT; PLUS; offset = INT; CLOSE_BRACKET { Chasm.word_ptr_of_r64_plus_r64_scaled_plus_offset (`r64 base) (`r64 index) (scale) (offset)}
    | WORD PTR? OPEN_BRACKET; base = r32; PLUS; index = r32; MULT; scale = INT; PLUS; offset = INT; CLOSE_BRACKET { Chasm.word_ptr_of_r32_plus_r32_scaled_plus_offset (`r32 base) (`r32 index) (scale) (offset)}
    (** [reg * scale + offset] *)
    | WORD PTR? OPEN_BRACKET; base = r64; MULT; scale = INT; PLUS; offset = INT; CLOSE_BRACKET { Chasm.word_ptr_of_r64_scaled_plus_offset (`r64 base) (scale) (offset)}
    | WORD PTR? OPEN_BRACKET; base = r32; MULT; scale = INT; PLUS; offset = INT; CLOSE_BRACKET { Chasm.word_ptr_of_r32_scaled_plus_offset (`r32 base) (scale) (offset)}

dword_ptr:
    (* [reg] *)
    | DWORD PTR? OPEN_BRACKET; r = r64; CLOSE_BRACKET { Chasm.dword_ptr_of_r64 (`r64 r) }
    | DWORD PTR? OPEN_BRACKET; r = r32; CLOSE_BRACKET { Chasm.dword_ptr_of_r32 (`r32 r) }
    (* [reg + reg] *)
    | DWORD PTR? OPEN_BRACKET; base = r64; PLUS; index = r64; CLOSE_BRACKET { Chasm.dword_ptr_of_r64_plus_r64 (`r64 base) (`r64 index)}
    | DWORD PTR? OPEN_BRACKET; base = r32; PLUS; index = r32; CLOSE_BRACKET { Chasm.dword_ptr_of_r32_plus_r32 (`r32 base) (`r32 index)}
    (** [reg + reg * scale] *)
    | DWORD PTR? OPEN_BRACKET; base = r64; PLUS; index = r64; MULT; scale = INT; CLOSE_BRACKET { Chasm.dword_ptr_of_r64_plus_r64_scaled (`r64 base) (`r64 index) (scale)}
    | DWORD PTR? OPEN_BRACKET; base = r32; PLUS; index = r32; MULT; scale = INT; CLOSE_BRACKET { Chasm.dword_ptr_of_r32_plus_r32_scaled (`r32 base) (`r32 index) (scale)}
    (** [reg * scale] *)
    | DWORD PTR? OPEN_BRACKET; base = r64; MULT; scale = INT; CLOSE_BRACKET { Chasm.dword_ptr_of_r64_scaled (`r64 base) (scale)}
    | DWORD PTR? OPEN_BRACKET; base = r32; MULT; scale = INT; CLOSE_BRACKET { Chasm.dword_ptr_of_r32_scaled (`r32 base) (scale)}
    (** [reg + offset] *)
    | DWORD PTR? OPEN_BRACKET; base = r64; PLUS; offset = INT; CLOSE_BRACKET { Chasm.dword_ptr_of_r64_plus_offset (`r64 base) (offset)}
    | DWORD PTR? OPEN_BRACKET; base = r32; PLUS; offset = INT; CLOSE_BRACKET { Chasm.dword_ptr_of_r32_plus_offset (`r32 base) (offset)}
    (** [reg + reg + offset] *)
    | DWORD PTR? OPEN_BRACKET; base = r64; PLUS; index = r64; PLUS; offset = INT; CLOSE_BRACKET { Chasm.dword_ptr_of_r64_plus_r64_plus_offset (`r64 base) (`r64 index) (offset)}
    | DWORD PTR? OPEN_BRACKET; base = r32; PLUS; index = r32; PLUS; offset = INT; CLOSE_BRACKET { Chasm.dword_ptr_of_r32_plus_r32_plus_offset (`r32 base) (`r32 index) (offset)}
    (** [reg + reg * scale + offset] *)
    | DWORD PTR? OPEN_BRACKET; base = r64; PLUS; index = r64; MULT; scale = INT; PLUS; offset = INT; CLOSE_BRACKET { Chasm.dword_ptr_of_r64_plus_r64_scaled_plus_offset (`r64 base) (`r64 index) (scale) (offset)}
    | DWORD PTR? OPEN_BRACKET; base = r32; PLUS; index = r32; MULT; scale = INT; PLUS; offset = INT; CLOSE_BRACKET { Chasm.dword_ptr_of_r32_plus_r32_scaled_plus_offset (`r32 base) (`r32 index) (scale) (offset)}
    (** [reg * scale + offset] *)
    | DWORD PTR? OPEN_BRACKET; base = r64; MULT; scale = INT; PLUS; offset = INT; CLOSE_BRACKET { Chasm.dword_ptr_of_r64_scaled_plus_offset (`r64 base) (scale) (offset)}
    | DWORD PTR? OPEN_BRACKET; base = r32; MULT; scale = INT; PLUS; offset = INT; CLOSE_BRACKET { Chasm.dword_ptr_of_r32_scaled_plus_offset (`r32 base) (scale) (offset)}

qword_ptr:
    (* [reg] *)
    | QWORD PTR? OPEN_BRACKET; r = r64; CLOSE_BRACKET { Chasm.qword_ptr_of_r64 (`r64 r) }
    | QWORD PTR? OPEN_BRACKET; r = r32; CLOSE_BRACKET { Chasm.qword_ptr_of_r32 (`r32 r) }
    (* [reg + reg] *)
    | QWORD PTR? OPEN_BRACKET; base = r64; PLUS; index = r64; CLOSE_BRACKET { Chasm.qword_ptr_of_r64_plus_r64 (`r64 base) (`r64 index)}
    | QWORD PTR? OPEN_BRACKET; base = r32; PLUS; index = r32; CLOSE_BRACKET { Chasm.qword_ptr_of_r32_plus_r32 (`r32 base) (`r32 index)}
    (** [reg + reg * scale] *)
    | QWORD PTR? OPEN_BRACKET; base = r64; PLUS; index = r64; MULT; scale = INT; CLOSE_BRACKET { Chasm.qword_ptr_of_r64_plus_r64_scaled (`r64 base) (`r64 index) (scale)}
    | QWORD PTR? OPEN_BRACKET; base = r32; PLUS; index = r32; MULT; scale = INT; CLOSE_BRACKET { Chasm.qword_ptr_of_r32_plus_r32_scaled (`r32 base) (`r32 index) (scale)}
    (** [reg * scale] *)
    | QWORD PTR? OPEN_BRACKET; base = r64; MULT; scale = INT; CLOSE_BRACKET { Chasm.qword_ptr_of_r64_scaled (`r64 base) (scale)}
    | QWORD PTR? OPEN_BRACKET; base = r32; MULT; scale = INT; CLOSE_BRACKET { Chasm.qword_ptr_of_r32_scaled (`r32 base) (scale)}
    (** [reg + offset] *)
    | QWORD PTR? OPEN_BRACKET; base = r64; PLUS; offset = INT; CLOSE_BRACKET { Chasm.qword_ptr_of_r64_plus_offset (`r64 base) (offset)}
    | QWORD PTR? OPEN_BRACKET; base = r32; PLUS; offset = INT; CLOSE_BRACKET { Chasm.qword_ptr_of_r32_plus_offset (`r32 base) (offset)}
    (** [reg + reg + offset] *)
    | QWORD PTR? OPEN_BRACKET; base = r64; PLUS; index = r64; PLUS; offset = INT; CLOSE_BRACKET { Chasm.qword_ptr_of_r64_plus_r64_plus_offset (`r64 base) (`r64 index) (offset)}
    | QWORD PTR? OPEN_BRACKET; base = r32; PLUS; index = r32; PLUS; offset = INT; CLOSE_BRACKET { Chasm.qword_ptr_of_r32_plus_r32_plus_offset (`r32 base) (`r32 index) (offset)}
    (** [reg + reg * scale + offset] *)
    | QWORD PTR? OPEN_BRACKET; base = r64; PLUS; index = r64; MULT; scale = INT; PLUS; offset = INT; CLOSE_BRACKET { Chasm.qword_ptr_of_r64_plus_r64_scaled_plus_offset (`r64 base) (`r64 index) (scale) (offset)}
    | QWORD PTR? OPEN_BRACKET; base = r32; PLUS; index = r32; MULT; scale = INT; PLUS; offset = INT; CLOSE_BRACKET { Chasm.qword_ptr_of_r32_plus_r32_scaled_plus_offset (`r32 base) (`r32 index) (scale) (offset)}
    (** [reg * scale + offset] *)
    | QWORD PTR? OPEN_BRACKET; base = r64; MULT; scale = INT; PLUS; offset = INT; CLOSE_BRACKET { Chasm.qword_ptr_of_r64_scaled_plus_offset (`r64 base) (scale) (offset)}
    | QWORD PTR? OPEN_BRACKET; base = r32; MULT; scale = INT; PLUS; offset = INT; CLOSE_BRACKET { Chasm.qword_ptr_of_r32_scaled_plus_offset (`r32 base) (scale) (offset)}

immediate:
    | i = INT {(`imm i)}
    | DOLLAR_SIGN; i = IDENTIFIER {(`imm_runtime i)}
    | DOLLAR_SIGN; OPEN_BRACE; i = IDENTIFIER; CLOSE_BRACE {(`imm_runtime i)}

immediate_64:
    | i = INT64 {(`imm64 i)}
    (* TODO - imm64_runtime as above*)

instruction:
    | PUSH; r = r16 {Chasm__Chasm_types.Push (`r16 r)}
    | PUSH; r = r64 {Chasm__Chasm_types.Push (`r64 r)}
    | PUSH; i = immediate {Chasm__Chasm_types.Push i}
    | PUSH; a = qword_ptr {Chasm__Chasm_types.Push a}
    | PUSH; a = word_ptr {Chasm__Chasm_types.Push a}

    | JMP; i = IDENTIFIER {Chasm__Chasm_types.Jmp (`short_label i)}
    | JMP; LONG_LABEL; OPEN_PAREN; i = IDENTIFIER; CLOSE_PAREN {Chasm__Chasm_types.Jmp (`long_label i)}

    | SUB; r = r8;  COMMA; i = immediate {Chasm__Chasm_types.Sub(`r8  r, i)}
    | SUB; r = r16; COMMA; i = immediate {Chasm__Chasm_types.Sub(`r16 r, i)}
    | SUB; r = r32; COMMA; i = immediate {Chasm__Chasm_types.Sub(`r32 r, i)}
    | SUB; r = r64; COMMA; i = immediate {Chasm__Chasm_types.Sub(`r64 r, i)}

    | SUB; r1 = r8;  COMMA; r2 = r8  {Chasm__Chasm_types.Sub(`r8  r1, `r8  r2)}
    | SUB; r1 = r16; COMMA; r2 = r16 {Chasm__Chasm_types.Sub(`r16 r1, `r16 r2)}
    | SUB; r1 = r32; COMMA; r2 = r32 {Chasm__Chasm_types.Sub(`r32 r1, `r32 r2)}
    | SUB; r1 = r64; COMMA; r2 = r64 {Chasm__Chasm_types.Sub(`r64 r1, `r64 r2)}

    | SUB; r = r8;  COMMA; a = byte_ptr  {Chasm__Chasm_types.Sub(`r8  r, a)}
    | SUB; r = r16; COMMA; a = word_ptr  {Chasm__Chasm_types.Sub(`r16 r, a)}
    | SUB; r = r32; COMMA; a = dword_ptr {Chasm__Chasm_types.Sub(`r32 r, a)}
    | SUB; r = r64; COMMA; a = qword_ptr {Chasm__Chasm_types.Sub(`r64 r, a)}

    | SUB; a = byte_ptr;  COMMA; r = r8;  {Chasm__Chasm_types.Sub(a, `r8  r)}
    | SUB; a = word_ptr;  COMMA; r = r16; {Chasm__Chasm_types.Sub(a, `r16 r)}
    | SUB; a = dword_ptr; COMMA; r = r32; {Chasm__Chasm_types.Sub(a, `r32 r)}
    | SUB; a = qword_ptr; COMMA; r = r64; {Chasm__Chasm_types.Sub(a, `r64 r)}

    | MOV; r = r64; COMMA; i = immediate    {Chasm__Chasm_types.Mov(`r64 r, i)}
    | MOV; r = r64; COMMA; i = immediate_64 {Chasm__Chasm_types.Mov(`r64 r, i)}
line:
    | name = IDENTIFIER; COLON { Chasm__Chasm_types.Label name }
    | i = instruction {Chasm__Chasm_types.Instruction i}

program:
    | lines = line* EOF { lines }