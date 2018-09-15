module Lib where

import Text.ParserCombinators.Parsec
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Char

data OpCode = MOV | XCHG | LEA | PUSH | POP
            | PUSHF | POPF | XLAT | ADD | ADC
            | SUB | SBB | IMUL | MUL | IDIV
            | DIV | CBW | CWD | NEG | NOT | INC
            | DEC | AND | OR | XOR | SHR | SAR
            | SHL | ROL | ROR | RCL
            | RCR | TEST | CMP | STD | CLD | STC
            | CLC | CMC | LOOP | LOOPZ
            | LOOPNZ | REP
            | MOVS | LODS | STOS | SCAS | CMPS
            | JCC | JMP | CALL | RET | SYS
            deriving (Show, Eq, Enum, Bounded)

data Reg16 = AX | BX | CX | DX
    deriving (Show, Eq, Enum, Bounded)

data Reg8 = AH | AL | BH | BL | CH | CL | DH | DL
    deriving (Show, Eq, Enum, Bounded)

data SegReg = CS | DS | SS | ES
    deriving (Show, Eq, Enum, Bounded)

data PointerReg = SP | BP | SI | DI
    deriving (Show, Eq, Enum, Bounded)

newtype MemoryVar = MemVar String
    deriving (Show, Eq)

data Oper = OPR16 Reg16 | OPR8 Reg8 | OPSR SegReg
          | OPPR PointerReg | OPM MemoryVar | OPI Integer
    deriving (Show, Eq)

data Instr = I0 OpCode
           | I1 OpCode Oper
           | I2 OpCode Oper Oper
    deriving (Show, Eq)

idToReg16Table =
    [ ("AX", AX)
    , ("BX", BX)
    , ("CX", CX)
    , ("DX", DX)
    ]

idToReg8Table =
    [ ("AH", AH)
    , ("AL", AL)
    , ("BH", BH)
    , ("BL", BL)
    , ("CH", CH)
    , ("CL", CL)
    , ("DH", DH)
    , ("DL", DL)
    ]

idToSegRegTable =
    [ ("CS", CS)
    , ("DS", DS)
    , ("SS", SS)
    , ("ES", ES)
    ]

idToPointerRegTable =
    [ ("SP", SP)
    , ("BP", BP)
    , ("SI", SI)
    , ("DI", DI)
    ]

idToInstrTable =
    [ ("MOV", MOV)
    , ("XCHG", XCHG)
    , ("LEA", LEA)
    , ("PUSH", PUSH)
    , ("POP", POP)
    , ("PUSHF", PUSHF)
    , ("POPF", POPF)
    , ("XLAT", XLAT)
    , ("ADD", ADD)
    , ("ADC", ADC)
    , ("SUB", SUB)
    , ("SBB", SBB)
    , ("IMUL", IMUL)
    , ("MUL", MUL)
    , ("IDIV", IDIV)
    , ("DIV", DIV)
    , ("CBW", CBW)
    , ("CWD", CWD)
    , ("NEG", NEG)
    , ("NOT", NOT)
    , ("INC", INC)
    , ("DEC", DEC)
    , ("AND", AND)
    , ("OR", OR)
    , ("XOR", XOR)
    , ("SHR", SHR)
    , ("SAR", SAR)
    , ("SAL", SHL)
    , ("SHL", SHL)
    , ("ROL", ROL)
    , ("ROR", ROR)
    , ("RCL", RCL)
    , ("RCR", RCR)
    , ("TEST", TEST)
    , ("CMP", CMP)
    , ("STD", STD)
    , ("CLD", CLD)
    , ("STC", STC)
    , ("CLC", CLC)
    , ("CMC", CMC)
    , ("LOOP", LOOP)
    , ("LOOPZ", LOOPZ)
    , ("LOOPE", LOOPZ)
    , ("LOOPNZ", LOOPNZ)
    , ("LOOPNE", LOOPNZ)
    , ("REP", REP)
    , ("REPZ", REP)
    , ("REPNZ", REP)
    , ("MOVS", MOVS)
    , ("LODS", LODS)
    , ("STOS", STOS)
    , ("SCAS", SCAS)
    , ("CMPS", CMPS)
    , ("JCC", JCC)
    , ("JMP", JMP)
    , ("CALL", CALL)
    , ("RET", RET)
    , ("SYS", SYS)
    ]

arity :: [(OpCode, Int)]
arity =
    [ (MOV, 2)
    , (XCHG, 2)
    , (LEA, 2)
    , (PUSH, 1)
    , (POP, 1)
    , (PUSHF, 0)
    , (POPF, 0)
    , (XLAT, 0)
    , (ADD, 2)
    , (ADC, 2)
    , (SUB, 2)
    , (SBB, 2)
    , (IMUL, 1)
    , (MUL, 1)
    , (IDIV, 1)
    , (DIV, 1)
    , (CBW, 0)
    , (CWD, 0)
    , (NEG, 1)
    , (NOT, 1)
    , (INC, 1)
    , (DEC, 1)
    , (AND, 2)
    , (OR, 2)
    , (XOR, 2)
    , (SHR, 2)
    , (SAR, 2)
    , (SHL, 2)
    , (ROL, 2)
    , (ROR, 2)
    , (RCL, 2)
    , (RCR, 2)
    , (TEST, 2)
    , (CMP, 2)
    , (STD, 0)
    , (CLD, 0)
    , (STC, 0)
    , (CLC, 0)
    , (CMC, 0)
    , (LOOP, 1)
    , (LOOPZ, 1)
    , (LOOPNZ, 1)
    , (REP, 1)
    , (MOVS, 0)
    , (LODS, 0)
    , (STOS, 0)
    , (SCAS, 0)
    , (CMPS, 0)
    , (JCC, 1)
    , (JMP, 1)
    , (CALL, 1)
    , (RET, 1)
    , (SYS, 0)
    ]

asmDef = emptyDef
    { commentLine = ";"
    , identStart = letter <|> oneOf "()"
    , identLetter = alphaNum <|> oneOf "()"
    , caseSensitive = False
    }

eol :: GenParser Char st Char
eol = char '\n'

lexer = makeTokenParser asmDef
identifier = Text.Parsec.Token.identifier lexer -- parses an identifier
parens     = Text.Parsec.Token.parens     lexer -- parses surrounding parenthesis:
                                                -- parens p
                                                -- takes care of the parenthesis and
                                                -- uses p to parse what's inside them
integer    = Text.Parsec.Token.integer    lexer -- parses an integer
semi       = Text.Parsec.Token.semi       lexer -- parses a semicolon
whiteSpace = Text.Parsec.Token.whiteSpace lexer -- parses whitespace
comma      = Text.Parsec.Token.comma      lexer -- parses comma
symbol     = Text.Parsec.Token.symbol     lexer -- parses symbol

parseInstr = foldl1 (<|>) (map f idToInstrTable)
    where
        f (name, proxy) = reserved lexer name >> return proxy

parseReg16 = foldl1 (<|>) (map f idToReg16Table)
    where
        f (name, proxy) = reserved lexer name >> return proxy

parseReg8 = foldl1 (<|>) (map f idToReg8Table)
    where
        f (name, proxy) = reserved lexer name >> return proxy

parseSegReg = foldl1 (<|>) (map f idToSegRegTable)
    where
        f (name, proxy) = reserved lexer name >> return proxy

parsePointerReg = foldl1 (<|>) (map f idToPointerRegTable)
    where
        f (name, proxy) = reserved lexer name >> return proxy

parseMem = Lib.identifier

parseOp = (OPR16 <$> parseReg16) <|> (OPR8 <$> parseReg8)
       <|> (OPSR <$> parseSegReg) <|> (OPPR <$> parsePointerReg)
       <|> (OPM . MemVar <$> parseMem) <|> (OPI <$> Lib.integer)

parseStmt = do
    instr <- parseInstr
    let ari = lookup instr arity
    operands <- commaSep1 lexer parseOp <* Lib.whiteSpace
    case ari of
        Just 0 -> return $ I0 instr
        Just 1 -> return $ I1 instr (head operands)
        Just 2 -> return $ I2 instr (head operands) (head . tail $ operands)
        _      -> error ""

data Clocks a = !a :+ !a
    deriving (Eq, Show, Read)

clocks :: Clocks a -> a
clocks (x :+ _) = x

ea :: Clocks a -> a
ea (_ :+ y) = y

instance Num a => Semigroup (Clocks a) where
    (x1 :+ y1) <> (x2 :+ y2) = (x1 + x2) :+ (y1 + y2)
