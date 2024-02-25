open Stdint

type r8  = | Al  | Cl  | Dl  | Bl  | Sil | Dil | Spl | Bpl | R8b | R9b | R10b | R11b | R12b | R13b | R14b | R15b
type r16 = | Ax  | Cx  | Dx  | Bx  | Si  | Di  | Sp  | Bp  | R8w | R9w | R10w | R11w | R12w | R13w | R14w | R15w
type r32 = | Eax | Ecx | Edx | Ebx | Esi | Edi | Esp | Ebp | R8d | R9d | R10d | R11d | R12d | R13d | R14d | R15d
type r64 = | Rax | Rcx | Rdx | Rbx | Rsi | Rdi | Rsp | Rbp | R8  | R9  | R10  | R11  | R12  | R13  | R14  | R15

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

type push_type = [ `r16 of r16   | `r64 of r64 
                 | `mem16 of mem | `mem64 of mem
                 | `imm8 of int8 | `imm16 of int16 | `imm32 of int32 | `imm of int ]

type instruction =
    | Push of push_type