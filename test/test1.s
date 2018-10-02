.SECT .TEXT
        MOV     AX, (x)
        ADD     AX, (y)
        MOV     SI, arr
        add     si, 3
        add     cx, (arr+2)
        add     AX, -2(SI)
.SECT .DATA
x:     .WORD    2
y:     .WORD    3
arr:   .WORD    2, 3, 4
end:   .WORD    0
.SECT .BSS
