open Stdint

type r8 = | Al | Cl | Dl | Bl | Sil | Dil | Spl | Bpl | R8b | R9b | R10b | R11b | R12b | R13b | R14b | R15b

type r16 = | Ax | Cx | Dx | Bx | Si | Di | Sp | Bp | R8w | R9w | R10w | R11w | R12w | R13w | R14w | R15w

type r32 = | Eax | Ecx | Edx | Ebx | Esi | Edi | Esp | Ebp | R8d | R9d | R10d | R11d | R12d | R13d | R14d | R15d

type r64 = | Rax | Rcx | Rdx | Rbx | Rsi | Rdi | Rsp | Rbp | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

type modrm =
    | SingleR64 of r64
    | R64PlusR64 of r64 * r64

type register = [ `r8 of r8 | `r16 of r16 | `r32 of r32 | `r64 of r64 ]

type push_type = [ `r16 of r16 | `r64 of r64 
                 | `modrm of modrm
                 | `imm8 of int8 | `imm16 of int16 | `imm32 of int32 | `imm of int ]

type instruction =
    | Push of push_type