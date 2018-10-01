module Lib where

import           Data.Char
import           Text.Parsec
import           Text.Parsec.Language
import           Text.Parsec.String
import           Text.Parsec.Token
import           Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as Map
import Data.Functor.Identity

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
            deriving (Show, Eq)

data Reg16 = AX | BX | CX | DX
    deriving (Show, Eq)

data Reg8 = AH | AL | BH | BL | CH | CL | DH | DL
    deriving (Show, Eq)

data SegReg = CS | DS | SS | ES
    deriving (Show, Eq)

data PointerReg = SP | BP | SI | DI
    deriving (Show, Eq)

data Reg = Reg16 Reg16 | Reg8 Reg8 | SegReg SegReg | PointerReg PointerReg
    deriving (Show, Eq)

newtype MemoryVar = MemVar String
    deriving (Show, Eq)

newtype DirectAddressing = DirectAddressing Int
    deriving (Show, Eq)

newtype IndirectAddressing = IndirectAddressing Reg
    deriving (Show, Eq)

data RegisterOffsetAddressing = RegisterOffsetAddressing Int Reg
    deriving (Show, Eq)

data IndexRegisterAddressing = IndexRegisterAddressing Reg Reg
    deriving (Show, Eq)

data IndexRegisterOffsetAddressing = IndexRegisterOffsetAddressing Int Reg Reg
    deriving (Show, Eq)

data Oper = OPR Reg | OPM MemoryVar | OPI Integer
    deriving (Show, Eq)

data Instr = I0 OpCode
           | I1 OpCode Oper
           | I2 OpCode Oper Oper
    deriving (Show, Eq)

type Identifier = String

data Type = Byte | Word | ASCII
    deriving (Show, Eq)

typeTable =
    [ ("BYTE", Byte)
    , ("WORD", Word)
    , ("ASCII", ASCII)
    ]

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
    , identStart = letter
    , identLetter = alphaNum
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

parseReg = (Reg16 <$> parseReg16) <|> (Reg8 <$> parseReg8)
        <|> (SegReg <$> parseSegReg) <|> (PointerReg <$> parsePointerReg)

parseType = foldl1 (<|>) (map f typeTable)
    where
        f (name, proxy) = reserved lexer name >> return proxy

parseDataEntry = do
    Lib.whiteSpace
    ident <- manyTill anyChar (char ':')
    Lib.whiteSpace
    char '.'
    tp <- parseType
    Lib.whiteSpace
    return (ident, tp)

parseDataSection = do
    Lib.whiteSpace
    string ".SECT .DATA"
    list <- parseDataEntry `sepBy` eol
    Lib.whiteSpace
    return $ Map.fromList list

parseDirectAddressing :: ParsecT String () Identity DirectAddressing -- WTF?
parseDirectAddressing = DirectAddressing <$> (read <$> Lib.parens (many digit))

parseIndirectAddressing = IndirectAddressing <$> Lib.parens parseReg

parseRegisterOffsetAddressing = do
    offset <- many digit
    address <- Lib.parens parseReg
    return $ RegisterOffsetAddressing (read offset) address

parseIndexRegisterAddressing = do
    index <- Lib.parens parseReg
    address <- Lib.parens parseReg
    return $ IndexRegisterAddressing index address

parseIndexRegisterOffsetAddressing = do
    offset <- many digit
    index <- Lib.parens parseReg
    address <- Lib.parens parseReg
    return $ IndexRegisterOffsetAddressing (read offset) index address

parseMem = Lib.identifier

parseOp = (OPR <$> parseReg) <|> (OPM . MemVar <$> parseMem) <|> (OPI <$> Lib.integer)

parseStmt = do
    Lib.whiteSpace
    instr <- parseInstr
    let ari = lookup instr arity
    case ari of
        Just 0 -> return $ I0 instr
        Just 1 -> do operands <- commaSep1 lexer parseOp <* Lib.whiteSpace
                     return $ I1 instr (head operands)
        Just 2 -> do operands <- commaSep1 lexer parseOp <* Lib.whiteSpace
                     return $ I2 instr (head operands) (head . tail $ operands)
        _      -> error ""

-- in order to work with data one needs to parse them and
-- add to some kind of State [(Ident, Value)]
escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

parseString :: Parser String
parseString = do
    start <- character -- to avoid applying many to empty parser
    strings <- many character
    newline
    return $ concat strings

parseFile = do
    Lib.whiteSpace
    string ".SECT .TEXT"
    newline
    result <- many parseStmt
    Lib.whiteSpace
    optional parseDataSection
    Lib.whiteSpace
    optional $ string ".SECT .BSS"
    many parseString
    Lib.whiteSpace
    eof
    return result

data Clocks a = !a :+ !a
    deriving (Eq, Read)

instance Show a => Show (Clocks a) where
    show (x :+ y) = show x ++ " + " ++ show y ++ "EA"

instance Num a => Semigroup (Clocks a) where
    (x1 :+ y1) <> (x2 :+ y2) = (x1 + x2) :+ (y1 + y2)

clocksMap :: Instr -> Clocks Int
clocksMap instr = case instr of
  -- MOV
  I2 MOV (OPM _) (OPR (Reg16 AX))          -> 10 :+ 0
  I2 MOV (OPM _) (OPR (Reg8 AL))           -> 10 :+ 0
  I2 MOV (OPR (Reg16 AX)) (OPM _)          -> 10 :+ 0
  I2 MOV (OPR (Reg8 AL)) (OPM _)           -> 10 :+ 0
  I2 MOV (OPR (SegReg _)) (OPR (Reg16 _))  -> 2 :+ 0
  I2 MOV (OPR (SegReg _)) (OPM _)          -> 8 :+ 1
  I2 MOV (OPR (Reg16 _)) (OPR (SegReg _))  -> 2 :+ 0
  I2 MOV (OPM _) (OPR (SegReg _))          -> 9 :+ 1
  I2 MOV (OPR _) (OPR _)                   -> 2 :+ 0
  I2 MOV (OPR _) (OPM _)                   -> 8 :+ 1
  I2 MOV (OPM _) (OPR _)                   -> 9 :+ 1
  I2 MOV (OPR _) (OPI _)                   -> 4 :+ 0
  I2 MOV (OPM _) (OPI _)                   -> 10 :+ 1
  -- XCHG
  I2 XCHG (OPR (Reg8 AL)) (OPR (Reg16 _))  -> 3 :+ 0
  I2 XCHG (OPR (Reg16 AX)) (OPR (Reg16 _)) -> 3 :+ 0
  I2 XCHG (OPM _) (OPR _)                  -> 17 :+ 1
  I2 XCHG (OPR _) (OPR _)                  -> 4 :+ 0
  -- LEA
  I2 LEA (OPR (Reg16 _)) (OPM _)           -> 2 :+ 1
  -- PUSH
  I1 PUSH (OPR (SegReg _))                 -> 10 :+ 0
  I1 PUSH (OPR _)                          -> 11 :+ 0
  I1 PUSH (OPM _)                          -> 16 :+ 1
  -- POP
  I1 POP (OPR (SegReg _))                  -> 8 :+ 0
  I1 POP (OPR _)                           -> 8 :+ 0
  I1 POP (OPM _)                           -> 17 :+ 1
  -- PUSHF
  I0 PUSHF                                 -> 10 :+ 0
  -- POPF
  I0 POPF                                  -> 8 :+ 0
  -- XLAT
  I1 XLAT (OPM _)                          -> 11 :+ 0
  -- ADD
  I2 ADD (OPR (Reg8 AL)) (OPI _)           -> 4 :+ 0
  I2 ADD (OPR (Reg16 AX)) (OPI _)          -> 4 :+ 0
  I2 ADD (OPM _) (OPI _)                   -> 17 :+ 1
  I2 ADD (OPR _) (OPI _)                   -> 4 :+ 0
  I2 ADD (OPM _) (OPR _)                   -> 16 :+ 1
  I2 ADD (OPR _) (OPM _)                   -> 9 :+ 1
  I2 ADD (OPR _) (OPR _)                   -> 3 :+ 0
  -- ADC
  I2 ADC (OPR (Reg8 AL)) (OPI _)           -> 4 :+ 0
  I2 ADC (OPR (Reg16 AX)) (OPI _)          -> 4 :+ 0
  I2 ADC (OPM _) (OPI _)                   -> 17 :+ 1
  I2 ADC (OPR _) (OPI _)                   -> 4 :+ 0
  I2 ADC (OPM _) (OPR _)                   -> 16 :+ 1
  I2 ADC (OPR _) (OPM _)                   -> 9 :+ 1
  I2 ADC (OPR _) (OPR _)                   -> 3 :+ 0
  -- SUB
  I2 SUB (OPR (Reg8 AL)) (OPI _)           -> 4 :+ 0
  I2 SUB (OPR (Reg16 AX)) (OPI _)          -> 4 :+ 0
  I2 SUB (OPR _) (OPI _)                   -> 4 :+ 0
  I2 SUB (OPM _) (OPI _)                   -> 17 :+ 1
  I2 SUB (OPR _) (OPR _)                   -> 3 :+ 0
  I2 SUB (OPR _) (OPM _)                   -> 9 :+ 1
  I2 SUB (OPM _) (OPR _)                   -> 16 :+ 1
  -- SBB
  I2 SBB (OPR (Reg8 AL)) (OPI _)           -> 4 :+ 0
  I2 SBB (OPR (Reg16 AX)) (OPI _)          -> 4 :+ 0
  I2 SBB (OPR _) (OPI _)                   -> 4 :+ 0
  I2 SBB (OPM _) (OPI _)                   -> 17 :+ 1
  I2 SBB (OPM _) (OPR _)                   -> 16 :+ 1
  I2 SBB (OPR _) (OPM _)                   -> 9 :+ 1
  I2 SBB (OPR _) (OPR _)                   -> 3 :+ 0
  -- IMUL
  I1 IMUL (OPR (Reg8 _))                   -> 80 :+ 0
  I1 IMUL (OPR (Reg16 _))                  -> 128 :+ 0
  I1 IMUL (OPM _)                          -> 86 :+ 1
  -- MUL
  I1 MUL (OPR (Reg8 _))                    -> 70 :+ 0
  I1 MUL (OPR (Reg16 _))                   -> 118 :+ 0
  I1 MUL (OPM _)                           -> 76 :+ 1
  -- IDIV
  I1 IDIV (OPR (Reg8 _))                   -> 101 :+ 0
  I1 IDIV (OPR (Reg16 _))                  -> 165 :+ 0
  I1 IDIV (OPM _)                          -> 107 :+ 1
  -- DIV
  I1 DIV (OPR (Reg8 _))                    -> 80 :+ 0
  I1 DIV (OPR (Reg16 _))                   -> 144 :+ 0
  I1 DIV (OPM _)                           -> 86 :+ 1
  -- CBW
  I0 CBW                                   -> 2 :+ 0
  -- CWD
  I0 CWD                                   -> 5 :+ 0
  -- NEG
  I1 NEG (OPR _)                           -> 3 :+ 0
  I1 NEG (OPM _)                           -> 16 :+ 1
  -- NOT
  I1 NOT (OPR _)                           -> 3 :+ 0
  I1 NOT (OPM _)                           -> 16 :+ 1
  -- INC
  I1 INC (OPR (Reg8 _))                    -> 3 :+ 0
  I1 INC (OPR (Reg16 _))                   -> 2 :+ 0
  I1 INC (OPM _)                           -> 15 :+ 1
  -- DEC
  I1 DEC (OPR (Reg8 _))                    -> 3 :+ 0
  I1 DEC (OPR (Reg16 _))                   -> 2 :+ 0
  I1 DEC (OPM _)                           -> 15 :+ 1
  -- AND
  I2 AND (OPR (Reg8 AL)) (OPI _)           -> 4 :+ 0
  I2 AND (OPR (Reg16 AX)) (OPI _)          -> 4 :+ 0
  I2 AND (OPM _) (OPI _)                   -> 17 :+ 1
  I2 AND (OPR _) (OPI _)                   -> 4 :+ 0
  I2 AND (OPM _) (OPR _)                   -> 16 :+ 1
  I2 AND (OPR _) (OPM _)                   -> 9 :+ 1
  I2 AND (OPR _) (OPR _)                   -> 3 :+ 0
  -- OR
  I2 OR (OPR (Reg8 AL)) (OPI _)            -> 4 :+ 0
  I2 OR (OPR (Reg16 AX)) (OPI _)           -> 4 :+ 0
  I2 OR (OPM _) (OPI _)                    -> 17 :+ 1
  I2 OR (OPR _) (OPI _)                    -> 4 :+ 0
  I2 OR (OPM _) (OPR _)                    -> 16 :+ 1
  I2 OR (OPR _) (OPM _)                    -> 9 :+ 1
  I2 OR (OPR _) (OPR _)                    -> 3 :+ 0
  -- XOR
  I2 XOR (OPR (Reg8 AL)) (OPI _)           -> 4 :+ 0
  I2 XOR (OPR (Reg16 AX)) (OPI _)          -> 4 :+ 0
  I2 XOR (OPM _) (OPI _)                   -> 17 :+ 1
  I2 XOR (OPR _) (OPI _)                   -> 4 :+ 0
  I2 XOR (OPM _) (OPR _)                   -> 16 :+ 1
  I2 XOR (OPR _) (OPM _)                   -> 9 :+ 1
  I2 XOR (OPR _) (OPR _)                   -> 3 :+ 0
  -- SHR
  I2 SHR (OPR _) (OPI 1)                   -> 2 :+ 0
  I2 SHR (OPR _) (OPR (Reg8 CL))           -> 8 :+ 0
  I2 SHR (OPM _) (OPI 1)                   -> 15 :+ 1
  I2 SHR (OPM _) (OPR (Reg8 CL))           -> 20 :+ 1
  -- SAR
  I2 SAR (OPR _) (OPI 1)                   -> 2 :+ 0
  I2 SAR (OPR _) (OPR (Reg8 CL))           -> 8 :+ 0
  I2 SAR (OPM _) (OPI 1)                   -> 15 :+ 1
  I2 SAR (OPM _) (OPR (Reg8 CL))           -> 20 :+ 1
  -- SHL
  I2 SHL (OPR _) (OPI 1)                   -> 2 :+ 0
  I2 SHL (OPR _) (OPR (Reg8 CL))           -> 8 :+ 0
  I2 SHL (OPM _) (OPI 1)                   -> 15 :+ 1
  I2 SHL (OPM _) (OPR (Reg8 CL))           -> 20 :+ 1
  -- ROL
  I2 ROL (OPR _) (OPI 1)                   -> 2 :+ 0
  I2 ROL (OPR _) (OPR (Reg8 CL))           -> 8 :+ 0
  I2 ROL (OPM _) (OPI 1)                   -> 15 :+ 1
  I2 ROL (OPM _) (OPR (Reg8 CL))           -> 20 :+ 1
  -- ROR
  I2 ROR (OPR _) (OPI 1)                   -> 2 :+ 0
  I2 ROR (OPR _) (OPR (Reg8 CL))           -> 8 :+ 0
  I2 ROR (OPM _) (OPI 1)                   -> 15 :+ 1
  I2 ROR (OPM _) (OPR (Reg8 CL))           -> 20 :+ 1
  -- RCL
  I2 RCL (OPR _) (OPI 1)                   -> 2 :+ 0
  I2 RCL (OPR _) (OPR (Reg8 CL))           -> 8 :+ 0
  I2 RCL (OPM _) (OPI 1)                   -> 15 :+ 1
  I2 RCL (OPM _) (OPR (Reg8 CL))           -> 20 :+ 1
  -- RCR
  I2 RCR (OPR _) (OPI 1)                   -> 2 :+ 0
  I2 RCR (OPR _) (OPR (Reg8 CL))           -> 8 :+ 0
  I2 RCR (OPM _) (OPI 1)                   -> 15 :+ 1
  I2 RCR (OPM _) (OPR (Reg8 CL))           -> 20 :+ 1
  -- TEST
  I2 TEST (OPR (Reg8 AL)) (OPI _)          -> 4 :+ 0
  I2 TEST (OPR (Reg16 AX)) (OPI _)         -> 4 :+ 0
  I2 TEST (OPM _) (OPI _)                  -> 11 :+ 1
  I2 TEST (OPR _) (OPI _)                  -> 5 :+ 0
  I2 TEST (OPR _) (OPM _)                  -> 9 :+ 1
  I2 TEST (OPR _) (OPR _)                  -> 3 :+ 0
  -- CMP
  I2 CMP (OPR (Reg8 AL)) (OPI _)           -> 4 :+ 0
  I2 CMP (OPR (Reg16 AX)) (OPI _)          -> 4 :+ 0
  I2 CMP (OPM _) (OPI _)                   -> 10 :+ 1
  I2 CMP (OPR _) (OPI _)                   -> 4 :+ 0
  I2 CMP (OPM _) (OPR _)                   -> 9 :+ 1
  I2 CMP (OPR _) (OPM _)                   -> 9 :+ 1
  I2 CMP (OPR _) (OPR _)                   -> 3 :+ 0
  -- STD
  I0 STD                                   -> 2 :+ 0
  -- CLD
  I0 CLD                                   -> 2 :+ 0
  -- STC
  I0 STC                                   -> 2 :+ 0
  -- CLC
  I0 CLC                                   -> 2 :+ 0
  -- CMC
  I0 CMC                                   -> 2 :+ 0
  -- LOOP
  --I1 LOOP (OPM _) -> 17 :+ 0
  -- LOOPZ
  --I1 LOOPZ (OPM _) -> 18 :+ 0
  -- LOOPNZ
  --I1 LOOPNZ (OPM _) -> 19 :+ 0
  -- REP
  I0 REP                                   -> 2 :+ 0
  -- MOVS
  I2 MOVS (OPM _) (OPM _)                  -> 18 :+ 0
  -- LODS
  I1 LODS (OPM _)                          -> 12 :+ 0
  -- STOS
  I1 STOS (OPM _)                          -> 11 :+ 0
  -- SCAS
  I1 SCAS (OPM _)                          -> 15 :+ 0
  -- CMPS
  I2 CMPS (OPM _) (OPM _)                  -> 22 :+ 0
  -- JCC
  --skip
  -- JMP
  --skip
  -- CALL
  I1 CALL (OPR _)                          -> 16 :+ 0
  I1 CALL (OPM _)                          -> 21 :+ 1
  -- RET
  --skip
  -- SYS
  --skip
  _                                        -> error "wrong arguments"

countClocks :: [Instr] -> Clocks Int
countClocks = foldr fun (0 :+ 0)
  where
    fun val acc = acc <> clocksMap val

countClocksFromParsedFile fn = do
  cont <- parseFromFile parseFile fn
  return $ countClocks <$> cont
