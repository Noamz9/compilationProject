;nasm -f elf64 filename.s
;gcc inon.o -o filename

set disassembly-flavor intel
layout split
layout regs

(fold-left (lambda (x y) (cons y x)) '() '((1) (2 3)))