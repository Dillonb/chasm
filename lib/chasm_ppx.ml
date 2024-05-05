open Ppxlib
open Chasm__Chasm_types

let assemble asm_str =
    let lexbuf = Lexing.from_string asm_str in
       X64_parser.program X64_lexer.read lexbuf

let expr_of_option ~loc f o = match o with
  | None -> [%expr None]
  | Some x -> [%expr Some [%e f ~loc x]]

let expr_of_option_noloc ~loc f o = match o with
  | None -> [%expr None]
  | Some x -> [%expr Some [%e f x]]

let expr_of_r8 ~loc r = match r with
  | Al   -> [%expr `r8 Al]
  | Cl   -> [%expr `r8 Cl]
  | Dl   -> [%expr `r8 Dl]
  | Bl   -> [%expr `r8 Bl]
  | Sil  -> [%expr `r8 Sil]
  | Dil  -> [%expr `r8 Dil]
  | Spl  -> [%expr `r8 Spl]
  | Bpl  -> [%expr `r8 Bpl]
  | R8b  -> [%expr `r8 R8b]
  | R9b  -> [%expr `r8 R9b]
  | R10b -> [%expr `r8 R10b]
  | R11b -> [%expr `r8 R11b]
  | R12b -> [%expr `r8 R12b]
  | R13b -> [%expr `r8 R13b]
  | R14b -> [%expr `r8 R14b]
  | R15b -> [%expr `r8 R15b]
  | R8Runtime expr -> [%expr rq [%e Parse.expression (Lexing.from_string expr) ]]

let expr_of_r16 ~loc r = match r with
  | Ax   -> [%expr `r16 Ax]
  | Cx   -> [%expr `r16 Cx]
  | Dx   -> [%expr `r16 Dx]
  | Bx   -> [%expr `r16 Bx]
  | Si   -> [%expr `r16 Si]
  | Di   -> [%expr `r16 Di]
  | Sp   -> [%expr `r16 Sp]
  | Bp   -> [%expr `r16 Bp]
  | R8w  -> [%expr `r16 R8w]
  | R9w  -> [%expr `r16 R9w]
  | R10w -> [%expr `r16 R10w]
  | R11w -> [%expr `r16 R11w]
  | R12w -> [%expr `r16 R12w]
  | R13w -> [%expr `r16 R13w]
  | R14w -> [%expr `r16 R14w]
  | R15w -> [%expr `r16 R15w]
  | R16Runtime expr -> [%expr rq [%e Parse.expression (Lexing.from_string expr) ]]

let expr_of_r32 ~loc r = match r with
  | Eax  -> [%expr `r32 Eax]
  | Ecx  -> [%expr `r32 Ecx]
  | Edx  -> [%expr `r32 Edx]
  | Ebx  -> [%expr `r32 Ebx]
  | Esi  -> [%expr `r32 Esi]
  | Edi  -> [%expr `r32 Edi]
  | Esp  -> [%expr `r32 Esp]
  | Ebp  -> [%expr `r32 Ebp]
  | R8d  -> [%expr `r32 R8d]
  | R9d  -> [%expr `r32 R9d]
  | R10d -> [%expr `r32 R10d]
  | R11d -> [%expr `r32 R11d]
  | R12d -> [%expr `r32 R12d]
  | R13d -> [%expr `r32 R13d]
  | R14d -> [%expr `r32 R14d]
  | R15d -> [%expr `r32 R15d]
  | R32Runtime expr -> [%expr rq [%e Parse.expression (Lexing.from_string expr) ]]
let expr_of_r32_option ~loc r = expr_of_option ~loc expr_of_r32 r

let expr_of_r32_notag ~loc r = match r with
  | Eax  -> [%expr Eax]
  | Ecx  -> [%expr Ecx]
  | Edx  -> [%expr Edx]
  | Ebx  -> [%expr Ebx]
  | Esi  -> [%expr Esi]
  | Edi  -> [%expr Edi]
  | Esp  -> [%expr Esp]
  | Ebp  -> [%expr Ebp]
  | R8d  -> [%expr R8d]
  | R9d  -> [%expr R9d]
  | R10d -> [%expr R10d]
  | R11d -> [%expr R11d]
  | R12d -> [%expr R12d]
  | R13d -> [%expr R13d]
  | R14d -> [%expr R14d]
  | R15d -> [%expr R15d]
  | R32Runtime expr -> [%expr rq_notag [%e Parse.expression (Lexing.from_string expr) ]]
let expr_of_r32_notag_option ~loc r = expr_of_option ~loc expr_of_r32_notag r

let expr_of_r64 ~loc r = match r with
  | Rax -> [%expr `r64 Rax]
  | Rcx -> [%expr `r64 Rcx]
  | Rdx -> [%expr `r64 Rdx]
  | Rbx -> [%expr `r64 Rbx]
  | Rsi -> [%expr `r64 Rsi]
  | Rdi -> [%expr `r64 Rdi]
  | Rsp -> [%expr `r64 Rsp]
  | Rbp -> [%expr `r64 Rbp]
  | R8  -> [%expr `r64 R8]
  | R9  -> [%expr `r64 R9]
  | R10 -> [%expr `r64 R10]
  | R11 -> [%expr `r64 R11]
  | R12 -> [%expr `r64 R12]
  | R13 -> [%expr `r64 R13]
  | R14 -> [%expr `r64 R14]
  | R15 -> [%expr `r64 R15]
  | R64Runtime expr -> [%expr rq [%e Parse.expression (Lexing.from_string expr) ]]
let expr_of_r64_option ~loc r = expr_of_option ~loc expr_of_r64 r

let expr_of_r64_notag ~loc r = match r with
  | Rax -> [%expr Rax]
  | Rcx -> [%expr Rcx]
  | Rdx -> [%expr Rdx]
  | Rbx -> [%expr Rbx]
  | Rsi -> [%expr Rsi]
  | Rdi -> [%expr Rdi]
  | Rsp -> [%expr Rsp]
  | Rbp -> [%expr Rbp]
  | R8  -> [%expr R8]
  | R9  -> [%expr R9]
  | R10 -> [%expr R10]
  | R11 -> [%expr R11]
  | R12 -> [%expr R12]
  | R13 -> [%expr R13]
  | R14 -> [%expr R14]
  | R15 -> [%expr R15]
  | R64Runtime expr -> [%expr rq [%e Parse.expression (Lexing.from_string expr) ]]
let expr_of_r64_notag_option ~loc r = expr_of_option ~loc expr_of_r64_notag r

let expr_of_int i = Parse.expression (Lexing.from_string (string_of_int i))
let expr_of_int_option ~loc i = expr_of_option_noloc ~loc expr_of_int i

let expr_of_imm ~loc i = match i with
  | `imm8 i -> [%expr Stdint.Int8.of_int([%e expr_of_int (Stdint.Int8.to_int(i))])]
  | `imm16 i -> [%expr Stdint.Int16.of_int([%e expr_of_int (Stdint.Int16.to_int(i))])]
  | `imm32 i -> [%expr Stdint.Int32.of_int([%e expr_of_int (Stdint.Int32.to_int(i))])]
  | `imm64 i -> [%expr Stdint.Int64.of_int([%e expr_of_int (Stdint.Int64.to_int(i))])]
  | `imm i -> [%expr `imm [%e expr_of_int i]]
let expr_of_imm_option ~loc i = expr_of_option ~loc expr_of_imm i

let expr_of_r32ptr ~loc p = match p with
  | { base = base; index = index; scale = scale; offset = offset } ->
    [%expr {
        base = [%e expr_of_r32_notag_option ~loc base];
        index = [%e expr_of_r32_notag_option ~loc index];
        scale = [%e expr_of_int_option ~loc scale];
        offset = [%e expr_of_imm_option ~loc offset]
      }
    ]

let expr_of_r64ptr ~loc p = match p with
  | { base = base; index = index; scale = scale; offset = offset } ->
    [%expr {
        base = [%e expr_of_r64_notag_option ~loc base];
        index = [%e expr_of_r64_notag_option ~loc index];
        scale = [%e expr_of_int_option ~loc scale];
        offset = [%e expr_of_imm_option ~loc offset]
      }
    ]

let expr_of_mem8 ~loc m = match m with
  | R64Ptr p -> [%expr `mem8 (R64Ptr [%e expr_of_r64ptr ~loc p])]
  | R32Ptr p -> [%expr `mem8 (R32Ptr [%e expr_of_r32ptr ~loc p])]

let expr_of_mem16 ~loc m = match m with
  | R64Ptr p -> [%expr `mem16 (R64Ptr [%e expr_of_r64ptr ~loc p])]
  | R32Ptr p -> [%expr `mem16 (R32Ptr [%e expr_of_r32ptr ~loc p])]

let expr_of_mem32 ~loc m = match m with
  | R64Ptr p -> [%expr `mem32 (R64Ptr [%e expr_of_r64ptr ~loc p])]
  | R32Ptr p -> [%expr `mem32 (R32Ptr [%e expr_of_r32ptr ~loc p])]

let expr_of_mem64 ~loc m = match m with
  | R64Ptr p -> [%expr `mem64 (R64Ptr [%e expr_of_r64ptr ~loc p])]
  | R32Ptr p -> [%expr `mem64 (R32Ptr [%e expr_of_r32ptr ~loc p])]

let expr_of_arg ~loc arg = match arg with
  | `r8  r -> expr_of_r8  ~loc r
  | `r16 r -> expr_of_r16 ~loc r
  | `r32 r -> expr_of_r32 ~loc r
  | `r64 r -> expr_of_r64 ~loc r
  | `mem8  m -> expr_of_mem8  ~loc m
  | `mem16 m -> expr_of_mem16 ~loc m
  | `mem32 m -> expr_of_mem32 ~loc m
  | `mem64 m -> expr_of_mem64 ~loc m
  | `uimm8 _ -> raise (Invalid_argument "`uimm8 unimplemented in expr_of_arg")
  | `uimm16 _ -> raise (Invalid_argument "`uimm16 unimplemented in expr_of_arg")
  | `uimm32 _ -> raise (Invalid_argument "`uimm32 unimplemented in expr_of_arg")
  | `imm8 _ -> raise (Invalid_argument "`imm8 unimplemented in expr_of_arg")
  | `imm16 _ -> raise (Invalid_argument "`imm16 unimplemented in expr_of_arg")
  | `imm32 _ -> raise (Invalid_argument "`imm32 unimplemented in expr_of_arg")
  | `imm i -> expr_of_imm ~loc (`imm i)
  | `imm_runtime expr -> [%expr `imm [%e Parse.expression (Lexing.from_string expr) ]]
  | `long_label l -> Parse.expression (Lexing.from_string ("`long_label \"" ^ l ^ "\""))
  | `short_label l -> Parse.expression (Lexing.from_string ("`short_label \"" ^ l ^ "\""))

let expr_of_instruction ~loc i = match i with
  | Push arg -> [%expr Push [%e expr_of_arg ~loc arg]]
  | Jmp arg -> [%expr Jmp [%e expr_of_arg ~loc arg]]
  | Sub (arg1,arg2) -> [%expr Sub ([%e expr_of_arg ~loc arg1], [%e expr_of_arg ~loc arg2])]

let expr_of_asm_line ~loc line = match line with
  | Instruction i -> [%expr Instruction [%e expr_of_instruction ~loc i ]]
  | Label l -> [%expr Label [%e Ast_builder.Default.estring ~loc l]]

let rec expr_of_asm_lines ~loc lines =
  match lines with
  | line :: remaining -> [%expr [%e expr_of_asm_line ~loc line] :: [%e expr_of_asm_lines ~loc remaining]]
  | [] -> [%expr []]

let expand ~ctxt asm_str =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let maybe_result = try Ok(assemble asm_str) with
        | ex -> Error ex in
      match maybe_result with
                  | Ok(result) -> [%expr [%e expr_of_asm_lines ~loc result] ]
                  | Error(ex) -> let err = Location.error_extensionf ~loc "Error assembling: %s" (Printexc.to_string ex) in
                  Ast_builder.Default.pexp_extension ~loc err

let my_extension =
  Extension.V3.declare
    "chasm"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let rule = Ppxlib.Context_free.Rule.extension my_extension

let () =
  Driver.register_transformation
    ~rules:[rule]
    "chasm"