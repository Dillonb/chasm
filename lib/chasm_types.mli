open Stdint

type r8  = | Al  | Cl  | Dl  | Bl  | Sil | Dil | Spl | Bpl | R8b | R9b | R10b | R11b | R12b | R13b | R14b | R15b | R8Runtime  of string
type r16 = | Ax  | Cx  | Dx  | Bx  | Si  | Di  | Sp  | Bp  | R8w | R9w | R10w | R11w | R12w | R13w | R14w | R15w | R16Runtime of string
type r32 = | Eax | Ecx | Edx | Ebx | Esi | Edi | Esp | Ebp | R8d | R9d | R10d | R11d | R12d | R13d | R14d | R15d | R32Runtime of string
type r64 = | Rax | Rcx | Rdx | Rbx | Rsi | Rdi | Rsp | Rbp | R8  | R9  | R10  | R11  | R12  | R13  | R14  | R15  | R64Runtime of string

type 'a mem_data = {
    base: 'a option;
    index: 'a option;
    scale: int option;

    offset: [`imm8 of int8 | `imm32 of int32 | `imm of int ] option;
}

type mem_size_16_64 =
    | M16
    | M64

type mem =
    | R64Ptr of r64 mem_data
    | R32Ptr of r32 mem_data

type arg = [ `r8 of r8 | `r16 of r16 | `r32 of r32 | `r64 of r64 
           | `mem8 of mem | `mem16 of mem | `mem32 of mem | `mem64 of mem
           | `uimm8 of uint8 | `uimm16 of uint16 | `uimm32 of uint32
           | `imm8 of int8 | `imm16 of int16 | `imm32 of int32 | `imm64 of uint64 | `imm of int | `imm_runtime of string | `imm64_runtime of string
           | `long_label of string | `short_label of string ]

type instruction =
    | Push of arg
    (* Note: far jumps are not supported *)
    | Jmp of arg
    | Sub of arg*arg
    | Mov of arg*arg


type asm_line =
    | Instruction of instruction
    | Label of string