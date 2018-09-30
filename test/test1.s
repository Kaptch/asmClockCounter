.SECT .TEXT
    mov ax, 4
    mov x, ax
    mov bx, x
.SECT .DATA
x:  .WORD   42
y:  .WORD   25
.SECT .BSS
ans:    .SPACE  2
