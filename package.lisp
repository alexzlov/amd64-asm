; package.lisp
; Package definition for the assembler.

(defpackage "AMD64-ASM"
  (:nicknames "ASM")
  (:use "COMMON-LISP" "ITERATE" "CFFI")
  (:export "RUN-TESTS" "ASSEMBLE-AND-OUTPUT"))
