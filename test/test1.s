; comment
_PRINTF = 127
  _EXIT = 1 ; dgdgd
  ;dghf
.SECT .TEXT
        MOV     AX, (x)
        ADD     AX, (y) ; some comment
        MOV     SI, arr ;;
        add     si, 3
        add     cx, (arr+2)
        add     AX, -2(SI)
        add     2(BP)(SI), 25
.SECT .DATA
x:     .WORD    2
y:     .WORD    3
arr:   .WORD    2, 3, 4
end:   .WORD    0
.SECT .BSS
