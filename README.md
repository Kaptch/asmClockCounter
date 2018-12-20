# asmClockCounter

## Usage

```shell
usage: asmClockCounter <input file>

usage example:
    $ ./asmClockCounter test1.s
    ! comment
    _PRINTF = 127
    _EXIT = 1 ! dgdgd
    !dghf
    .SECT .TEXT
        MOV     AX, (x) : 10
        ADD     AX, (y) ! some comment : 15
        MOV     SI, arr !! : 14
        add     si, 3 : 4
    L1: add     cx, (arr+2) : 15
    L2:
        add     AX, -2(SI) : 18
        add     2(BP)(SI), 25 : 29
    .SECT .DATA
        x:     .WORD    2
        y:     .WORD    3
        arr:   .WORD    2, 3, 4
        end:   .WORD    0
    .SECT .BSS
    Total: 105
```
## TODO

* Набор тестов.

* Добавить релиз для Windows.

* Добавить парсер для меток. В данный момент они обрабатываются парсером идентификаторов памяти.
В таком случае также нужно будет внести изменения в функцию вычисления количества тактов.

* С помощью [Template Haskell](https://wiki.haskell.org/Template_Haskell) избавиться от шаблонного кода в idTo*.

* Заменить списки на более эффективные структуры данных.

* Данные о регистрах и инструкциях можно задавать с помощью конфигурационного файла.

* Для такого простого ассемблера можно написать полноценный трассер, отслеживая данные в регистрах и в памяти.
Это позволит корректно считать количество тактов в программах с ветвлениями и циклами.

## Developer's tutorial

В случае крайне маловероятных изменений в используемом [форке](https://github.com/Kaptch/parsec) [Parsec](https://github.com/haskell/parsec) их стоит подгружать с помощью
```shell
git pull --recurse-submodules
```
Сборка осуществляется с помощью [stack](https://docs.haskellstack.org/en/stable/README/)
