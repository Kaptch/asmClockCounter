module Lib where

import           Data.Char
import           Data.Functor.Identity
import qualified Data.Map.Strict               as Map
import           Text.Parsec
import           Text.Parsec.Language
import           Text.Parsec.String
import qualified Text.Parsec.Token             as Token
import           Text.ParserCombinators.Parsec hiding (try)

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

data GeneralReg = AX | BX | CX | DX
    deriving (Show, Eq)

data Reg16 = GR GeneralReg | SR SegReg | PR PointerReg
    deriving (Show, Eq)

data Reg8 = AH | AL | BH | BL | CH | CL | DH | DL
    deriving (Show, Eq)

data SegReg = CS | DS | SS | ES
    deriving (Show, Eq)

data PointerReg = SP | BP | SI | DI
    deriving (Show, Eq)

data Reg = Reg16 Reg16 | Reg8 Reg8
    deriving (Show, Eq)

newtype MemoryVar = MemVar String
    deriving (Show, Eq)

newtype DirectAddressing = DirectAddressing String
    deriving (Show, Eq)

newtype IndirectAddressing = IndirectAddressing Reg
    deriving (Show, Eq)

data RegisterOffsetAddressing = RegisterOffsetAddressing Integer Reg
    deriving (Show, Eq)

data IndexRegisterAddressing = IndexRegisterAddressing Reg Reg
    deriving (Show, Eq)

data IndexRegisterOffsetAddressing = IndexRegisterOffsetAddressing Integer Reg Reg
    deriving (Show, Eq)

data Memory = DA DirectAddressing | IA IndirectAddressing | ROA RegisterOffsetAddressing
            | IRA IndexRegisterAddressing | IROA IndexRegisterOffsetAddressing
    deriving (Show, Eq)

data Oper = OPR Reg | OPM Memory | OPI Integer | LBL String
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
    { Token.commentLine = ";"
    , Token.identStart = letter
    , Token.identLetter = alphaNum <|> oneOf "+-"
    , Token.caseSensitive = False
    }

lexer = Token.makeTokenParser asmDef
identifier = Token.identifier lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
semi       = Token.semi       lexer
whiteSpace = Token.whiteSpace lexer
comma      = Token.comma      lexer
symbol     = Token.symbol     lexer

regToVal (name, proxy) = Token.reserved lexer name >> return proxy

parseInstr = foldl1 (<|>) (map regToVal idToInstrTable)

parseReg16 = foldl1 (<|>) (map regToVal idToReg16Table)

parseReg8 = foldl1 (<|>) (map regToVal idToReg8Table)

parseSegReg = foldl1 (<|>) (map regToVal idToSegRegTable)

parsePointerReg = foldl1 (<|>) (map regToVal idToPointerRegTable)

parseReg = (Reg16 . GR <$> parseReg16) <|> (Reg8 <$> parseReg8)
        <|> (Reg16 . SR <$> parseSegReg) <|> (Reg16 . PR <$> parsePointerReg)

appendBrackets str = '(' : str ++ ")"

parseDirectAddressing = do
    str <- identifier <|> (appendBrackets <$> parens identifier)
                    <|> (appendBrackets <$> (show <$> parens integer))
    return $ DirectAddressing str

parseIndirectAddressing = IndirectAddressing <$> parens parseReg

parseRegisterOffsetAddressing = do
    offset <- integer
    address <- parens parseReg
    return $ RegisterOffsetAddressing offset address

parseIndexRegisterAddressing = do
    index <- parens parseReg
    address <- parens parseReg
    return $ IndexRegisterAddressing index address

parseIndexRegisterOffsetAddressing = do
    offset <- integer
    index <- parens parseReg
    address <- parens parseReg
    return $ IndexRegisterOffsetAddressing offset index address

parseMemory = (IRA <$> try parseIndexRegisterAddressing)
           <|> (IROA <$> try parseIndexRegisterOffsetAddressing)
           <|> (IA <$> try parseIndirectAddressing)
           <|> (ROA <$> try parseRegisterOffsetAddressing)
           <|> (DA <$> try parseDirectAddressing)

parseOp = (OPR <$> parseReg) <|> (OPM <$> parseMemory) <|> (OPI <$> integer)

parseStmt = do
    whiteSpace
    instr <- parseInstr
    let ari = lookup instr arity
    case ari of
        Just 0 -> return $ I0 instr
        Just 1 -> do operands <- Token.commaSep1 lexer parseOp <* whiteSpace
                     return $ I1 instr (head operands)
        Just 2 -> do operands <- Token.commaSep1 lexer parseOp <* whiteSpace
                     return $ I2 instr (head operands) (head . tail $ operands)
        _      -> error ""

parseFile = do
    whiteSpace
    manyTill anyChar (try (string ".SECT .TEXT"))
    whiteSpace
    many parseStmt

ea a = case a of
  (DA _) -> 6
  (IA _) -> 5
  (ROA _) -> 9
  (IRA (IndexRegisterAddressing (Reg16 (PR BP)) (Reg16 (PR DI)))) -> 7
  (IRA (IndexRegisterAddressing (Reg16 (GR BX)) (Reg16 (PR SI)))) -> 7
  (IRA (IndexRegisterAddressing (Reg16 (PR BP)) (Reg16 (PR SI)))) -> 8
  (IRA (IndexRegisterAddressing (Reg16 (GR BX)) (Reg16 (PR DI)))) -> 8
  (IROA (IndexRegisterOffsetAddressing _ (Reg16 (PR BP)) (Reg16 (PR DI)))) -> 11
  (IROA (IndexRegisterOffsetAddressing _ (Reg16 (GR BX)) (Reg16 (PR SI)))) -> 11
  (IROA (IndexRegisterOffsetAddressing _ (Reg16 (PR BP)) (Reg16 (PR SI)))) -> 12
  (IROA (IndexRegisterOffsetAddressing _ (Reg16 (GR BX)) (Reg16 (PR DI)))) -> 12

clocksMap :: Instr -> Int
clocksMap instr = case instr of
  -- MOV
  I2 MOV (OPM _) (OPR (Reg16 (GR AX)))          -> 10
  I2 MOV (OPM _) (OPR (Reg8 AL))                -> 10
  I2 MOV (OPR (Reg16 (GR AX))) (OPM _)          -> 10
  I2 MOV (OPR (Reg8 AL)) (OPM _)                -> 10
  I2 MOV (OPR (Reg16 (SR _))) (OPR (Reg16 _))   -> 2
  I2 MOV (OPR (Reg16 (SR _))) (OPM a)           -> 8 + ea a
  I2 MOV (OPR (Reg16 _)) (OPR (Reg16 (SR _)))   -> 2
  I2 MOV (OPM a) (OPR (Reg16 (SR _)))           -> 9 + ea a
  I2 MOV (OPR _) (OPR _)                        -> 2
  I2 MOV (OPR _) (OPM a)                        -> 8 + ea a
  I2 MOV (OPM a) (OPR _)                        -> 9 + ea a
  I2 MOV (OPR _) (OPI _)                        -> 4
  I2 MOV (OPM a) (OPI _)                        -> 10 + ea a
  -- XCHG
  I2 XCHG (OPR (Reg8 AL)) (OPR (Reg16 _))       -> 3
  I2 XCHG (OPR (Reg16 (GR AX))) (OPR (Reg16 _)) -> 3
  I2 XCHG (OPM a) (OPR _)                       -> 17 + ea a
  I2 XCHG (OPR _) (OPR _)                       -> 4
  -- LEA
  I2 LEA (OPR (Reg16 _)) (OPM a)                -> 2 + ea a
  -- PUSH
  I1 PUSH (OPR (Reg16 (SR _)))                  -> 10
  I1 PUSH (OPR _)                               -> 11
  I1 PUSH (OPM a)                               -> 16 + ea a
  -- POP
  I1 POP (OPR (Reg16 (SR _)))                   -> 8
  I1 POP (OPR _)                                -> 8
  I1 POP (OPM a)                                -> 17 + ea a
  -- PUSHF
  I0 PUSHF                                      -> 10
  -- POPF
  I0 POPF                                       -> 8
  -- XLAT
  I1 XLAT (OPM _)                               -> 11
  -- ADD
  I2 ADD (OPR (Reg8 AL)) (OPI _)                -> 4
  I2 ADD (OPR (Reg16 (GR AX))) (OPI _)          -> 4
  I2 ADD (OPM a) (OPI _)                        -> 17 + ea a
  I2 ADD (OPR _) (OPI _)                        -> 4
  I2 ADD (OPM a) (OPR _)                        -> 16 + ea a
  I2 ADD (OPR _) (OPM a)                        -> 9 + ea a
  I2 ADD (OPR _) (OPR _)                        -> 3
  -- ADC
  I2 ADC (OPR (Reg8 AL)) (OPI _)                -> 4
  I2 ADC (OPR (Reg16 (GR AX))) (OPI _)          -> 4
  I2 ADC (OPM a) (OPI _)                        -> 17 + ea a
  I2 ADC (OPR _) (OPI _)                        -> 4
  I2 ADC (OPM a) (OPR _)                        -> 16 + ea a
  I2 ADC (OPR _) (OPM a)                        -> 9 + ea a
  I2 ADC (OPR _) (OPR _)                        -> 3
  -- SUB
  I2 SUB (OPR (Reg8 AL)) (OPI _)                -> 4
  I2 SUB (OPR (Reg16 (GR AX))) (OPI _)          -> 4
  I2 SUB (OPR _) (OPI _)                        -> 4
  I2 SUB (OPM a) (OPI _)                        -> 17 + ea a
  I2 SUB (OPR _) (OPR _)                        -> 3
  I2 SUB (OPR _) (OPM a)                        -> 9 + ea a
  I2 SUB (OPM a) (OPR _)                        -> 16 + ea a
  -- SBB
  I2 SBB (OPR (Reg8 AL)) (OPI _)                -> 4
  I2 SBB (OPR (Reg16 (GR AX))) (OPI _)          -> 4
  I2 SBB (OPR _) (OPI _)                        -> 4
  I2 SBB (OPM a) (OPI _)                        -> 17 + ea a
  I2 SBB (OPM a) (OPR _)                        -> 16 + ea a
  I2 SBB (OPR _) (OPM a)                        -> 9 + ea a
  I2 SBB (OPR _) (OPR _)                        -> 3
  -- IMUL
  I1 IMUL (OPR (Reg8 _))                        -> 98
  I1 IMUL (OPR (Reg16 _))                       -> 154
  I1 IMUL (OPM a)                               -> 104 + ea a
  -- MUL
  I1 MUL (OPR (Reg8 _))                         -> 77
  I1 MUL (OPR (Reg16 _))                        -> 133
  I1 MUL (OPM a)                                -> 139 + ea a
  -- IDIV
  I1 IDIV (OPR (Reg8 _))                        -> 112
  I1 IDIV (OPR (Reg16 _))                       -> 184
  I1 IDIV (OPM a)                               -> 190 + ea a
  -- DIV
  I1 DIV (OPR (Reg8 _))                         -> 90
  I1 DIV (OPR (Reg16 _))                        -> 162
  I1 DIV (OPM a)                                -> 168 + ea a
  -- CBW
  I0 CBW                                        -> 2
  -- CWD
  I0 CWD                                        -> 5
  -- NEG
  I1 NEG (OPR _)                                -> 3
  I1 NEG (OPM a)                                -> 16 + ea a
  -- NOT
  I1 NOT (OPR _)                                -> 3
  I1 NOT (OPM a)                                -> 16 + ea a
  -- INC
  I1 INC (OPR (Reg8 _))                         -> 3
  I1 INC (OPR (Reg16 _))                        -> 2
  I1 INC (OPM a)                                -> 15 + ea a
  -- DEC
  I1 DEC (OPR (Reg8 _))                         -> 3
  I1 DEC (OPR (Reg16 _))                        -> 2
  I1 DEC (OPM a)                                -> 15 + ea a
  -- AND
  I2 AND (OPR (Reg8 AL)) (OPI _)                -> 4
  I2 AND (OPR (Reg16 (GR AX))) (OPI _)          -> 4
  I2 AND (OPM a) (OPI _)                        -> 17 + ea a
  I2 AND (OPR _) (OPI _)                        -> 4
  I2 AND (OPM a) (OPR _)                        -> 16 + ea a
  I2 AND (OPR _) (OPM a)                        -> 9 + ea a
  I2 AND (OPR _) (OPR _)                        -> 3
  -- OR
  I2 OR (OPR (Reg8 AL)) (OPI _)                 -> 4
  I2 OR (OPR (Reg16 (GR AX))) (OPI _)           -> 4
  I2 OR (OPM a) (OPI _)                         -> 17 + ea a
  I2 OR (OPR _) (OPI _)                         -> 4
  I2 OR (OPM a) (OPR _)                         -> 16 + ea a
  I2 OR (OPR _) (OPM a)                         -> 9 + ea a
  I2 OR (OPR _) (OPR _)                         -> 3
  -- XOR
  I2 XOR (OPR (Reg8 AL)) (OPI _)                -> 4
  I2 XOR (OPR (Reg16 (GR AX))) (OPI _)          -> 4
  I2 XOR (OPM a) (OPI _)                        -> 17 + ea a
  I2 XOR (OPR _) (OPI _)                        -> 4
  I2 XOR (OPM a) (OPR _)                        -> 16 + ea a
  I2 XOR (OPR _) (OPM a)                        -> 9 + ea a
  I2 XOR (OPR _) (OPR _)                        -> 3
  -- SHR
  I2 SHR (OPR _) (OPI 1)                        -> 2
  I2 SHR (OPR _) (OPR (Reg8 CL))                -> 8
  I2 SHR (OPM a) (OPI 1)                        -> 15 + ea a
  I2 SHR (OPM a) (OPR (Reg8 CL))                -> 20 + ea a
  -- SAR
  I2 SAR (OPR _) (OPI 1)                        -> 2
  I2 SAR (OPR _) (OPR (Reg8 CL))                -> 8
  I2 SAR (OPM a) (OPI 1)                        -> 15 + ea a
  I2 SAR (OPM a) (OPR (Reg8 CL))                -> 20 + ea a
  -- SHL
  I2 SHL (OPR _) (OPI 1)                        -> 2
  I2 SHL (OPR _) (OPR (Reg8 CL))                -> 8
  I2 SHL (OPM a) (OPI 1)                        -> 15 + ea a
  I2 SHL (OPM a) (OPR (Reg8 CL))                -> 20 + ea a
  -- ROL
  I2 ROL (OPR _) (OPI 1)                        -> 2
  I2 ROL (OPR _) (OPR (Reg8 CL))                -> 8
  I2 ROL (OPM a) (OPI 1)                        -> 15 + ea a
  I2 ROL (OPM a) (OPR (Reg8 CL))                -> 20 + ea a
  -- ROR
  I2 ROR (OPR _) (OPI 1)                        -> 2
  I2 ROR (OPR _) (OPR (Reg8 CL))                -> 8
  I2 ROR (OPM a) (OPI 1)                        -> 15 + ea a
  I2 ROR (OPM a) (OPR (Reg8 CL))                -> 20 + ea a
  -- RCL
  I2 RCL (OPR _) (OPI 1)                        -> 2
  I2 RCL (OPR _) (OPR (Reg8 CL))                -> 8
  I2 RCL (OPM a) (OPI 1)                        -> 15 + ea a
  I2 RCL (OPM a) (OPR (Reg8 CL))                -> 20 + ea a
  -- RCR
  I2 RCR (OPR _) (OPI 1)                        -> 2
  I2 RCR (OPR _) (OPR (Reg8 CL))                -> 8
  I2 RCR (OPM a) (OPI 1)                        -> 15 + ea a
  I2 RCR (OPM a) (OPR (Reg8 CL))                -> 20 + ea a
  -- TEST
  I2 TEST (OPR (Reg8 AL)) (OPI _)               -> 4
  I2 TEST (OPR (Reg16 (GR AX))) (OPI _)         -> 4
  I2 TEST (OPM a) (OPI _)                       -> 11 + ea a
  I2 TEST (OPR _) (OPI _)                       -> 5
  I2 TEST (OPR _) (OPM a)                       -> 9 + ea a
  I2 TEST (OPR _) (OPR _)                       -> 3
  -- CMP
  I2 CMP (OPR (Reg8 AL)) (OPI _)                -> 4
  I2 CMP (OPR (Reg16 (GR AX))) (OPI _)          -> 4
  I2 CMP (OPM a) (OPI _)                        -> 10 + ea a
  I2 CMP (OPR _) (OPI _)                        -> 4
  I2 CMP (OPM a) (OPR _)                        -> 9 + ea a
  I2 CMP (OPR _) (OPM a)                        -> 9 + ea a
  I2 CMP (OPR _) (OPR _)                        -> 3
  -- STD
  I0 STD                                        -> 2
  -- CLD
  I0 CLD                                        -> 2
  -- STC
  I0 STC                                        -> 2
  -- CLC
  I0 CLC                                        -> 2
  -- CMC
  I0 CMC                                        -> 2
  -- LOOP
  --I1 LOOP (OPM _) -> 17 :+ 0
  -- LOOPZ
  --I1 LOOPZ (OPM _) -> 18 :+ 0
  -- LOOPNZ
  --I1 LOOPNZ (OPM _) -> 19 :+ 0
  -- REP
  I0 REP                                        -> 2
  -- MOVS
  I2 MOVS (OPM _) (OPM _)                       -> 18
  -- LODS
  I1 LODS (OPM _)                               -> 12
  -- STOS
  I1 STOS (OPM _)                               -> 11
  -- SCAS
  I1 SCAS (OPM _)                               -> 15
  -- CMPS
  I2 CMPS (OPM _) (OPM _)                       -> 22
  -- JCC
  --skip
  -- JMP
  --skip
  -- CALL
  I1 CALL (OPR _)                               -> 16
  I1 CALL (OPM a)                               -> 21 + ea a
  -- RET
  --skip
  -- SYS
  --skip
  _                                             -> 0 -- in order to process other instructions

countClocks :: [Instr] -> Int
countClocks = foldr fun 0
  where
    fun val acc = acc + clocksMap val

countClocksFromParsedFile fn = do
  cont <- parseFromFile parseFile fn
  return $ countClocks <$> cont
