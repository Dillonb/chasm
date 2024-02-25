#include <capstone/capstone.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>

csh handle = 0;

CAMLprim value disassemble(value code, value length) {
    CAMLparam2(code, length);

    unsigned char* code_ptr = Bytes_val(code);
    size_t code_size = Int_val(length);

    if (handle == 0) {
        if (cs_open(CS_ARCH_X86, CS_MODE_64, &handle) != CS_ERR_OK) {
            caml_failwith("Failed to initialize Capstone");
        }
    }
    cs_insn* insn;
    size_t count = cs_disasm(handle, code_ptr, code_size, 0, 1, &insn);
    if (count == 0) {
        caml_failwith("Failed to disassemble!");
    } else if (count == 1) {
        ssize_t buflen = snprintf(NULL, 0, "%s %s", insn[0].mnemonic, insn[0].op_str) + 1;
        char* buf = malloc(buflen);
        snprintf(buf, buflen, "%s %s", insn[0].mnemonic, insn[0].op_str);
        CAMLlocal1(result);
        result = caml_copy_string(buf);
        free(buf);
        CAMLreturn(result);
    } else {
        caml_failwith("Disassembling multiple instructions not supported");
    }
}