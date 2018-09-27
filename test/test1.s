.SECT .TEXT
    mov ax, x
    mov x, ax
    mov bx, ax
.SECT .DATA
x:  .WORD   42
y:  .WORD   25
.SECT .BSS
ans:    .SPACE  2
