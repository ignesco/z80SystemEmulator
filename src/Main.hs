module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import qualified Data.HashMap as H

import Data.Bits
import Data.List
import System.IO

pr1 = [halt]

prt1 = [
        ldA 0x34,
        cpA 0x34,
        cpA 0x88,
        jpNZ (NumberValue 0x0000),
        halt
    ]

pr2 = [
        Label "START" .>    ldA 0x4F,
                            outA 0x02,

                            ldA 0x0F,
                            outA 0x03,
        Label "LOOP1" .>    inA 0x00,
                            outA 0x01,

                            cpA 0x00,

                            jpNZ $ LabelValue "LOOP1",

                            nop,
        Label "END" .>      halt
    ]

pr3 = [
        ldRRnn BC 0x5499,
        ldRRnn HL 0x1234,
        ldRRnn SP 0xfedc,
        ldA 0xff,
        ld_nn_A 0x3265,
        ldA 0xee,
        ld_nn_A 0x3266,
        ldA_nn_ 0x3265,
        ldA_nn_ 0x3266,
        halt
    ]

pr4 = [
        ldRRnn SP 0x00ff,
        ldRRnn HL 0x1234,
        push HL,
        ldRRnn HL 0x0000,
        pop DE,
        halt
    ]

pr5 = [
                            ldRRnn SP 0x00ff,
                            call $ LabelValue "TheSub",
                            nop,
                            nop,
                            nop,
                            nop,
                            halt,

        Label "TheSub" .>   nop,
                            nop,
                            nop,
                            ret
    ]

pr6 = [
        ldRRnn SP 0x00ff,
        ldRRnn HL 0x1234,
        push HL,
        ldRR_nn_ DE (NumberValue 0x00fd),
        pop BC,
        halt
    ]

pr7 = [
        ldRRnn HL 0x9876,
        ld_nn_RR (NumberValue 0x00ff) HL,

        ldRR_nn_ DE (NumberValue 0x00ff),

        halt
    ]

pr8 = [
        ldRRnn HL 0x9876,
        ldrr H L,
        ldrr D L,
        halt
    ]

pr9 = [
        ldrn D 0x81,
        ldrn A 0x80,
        halt
    ]

pr10 = [
        ldrn A 0x10,
        ldrn B 0x13,
        addAr B,
        addAn 0x40,
        halt
    ]

pr11 = [
        ldrn A 0x89,
        ldRRnn HL 0x9876,
        ld_nn_RR (NumberValue 0x00ff) HL,
        ldRRnn HL 0x00ff,
        addA_HL_,
        halt
    ]

pr12 = [
        ldrn A 0x12,
        ldrn B 0x12,
        subAr B,
        subAn 0x40,
        halt
    ]

pr13 = [
        ldrn A 0x8a,
        ldRRnn HL 0x0011,
        ld_nn_RR (NumberValue 0x00ff) HL,
        ldRRnn HL 0x00ff,
        subA_HL_,
        halt
    ]

pr14 = [
        ldrn A 0xca,
        ldrn B 0x00,
        andAr B,
        halt
    ]

pr15 = [
        ldrn A 0xca,
        ldRRnn HL 0x0003,
        ld_nn_RR (NumberValue 0x00ff) HL,
        ldRRnn HL 0x00ff,
        andA_HL_,
        halt
    ]

pr16 = [
        ldrn A 0xca,
        ldrn B 0xca,
        cpr B,
        ldrn B 0xff,
        cpr B,
        halt
    ]

pr17 = [
        ldrn A 0xff,
        incr A,

        ldrn B 0xdd,
        decr B,

        halt
    ]

pr18 = [
        ldrn A 0xca,
        ld_nn_A 0x00ff,
        ldrn A 0x00,

        ldRRnn HL 0xff,

        dec_HL_,

        ldA_nn_ 0x00ff,

        halt
    ]

pr19 = [
        ldrn A 0x80,
        rrca,
        ldrn A 0x81,
        rrca,
        halt
    ]

pr20 = [
        ldrn A 0x80,
        rlca,
        ldrn A 0x81,
        rlca,
        halt
    ]

pr21 = [
        ldrn A 0x82,
        bitbr 2 A,
        bitbr 7 A,
        bitbr 6 A,
        bitbr 1 A,
        halt
    ]

pr22 = [
        ldrn A 0x82,
        ld_nn_A 0x00ff,
        ldRRnn HL 0x00ff,
        bitb_HL_ 2,
        bitb_HL_ 7,
        bitb_HL_ 6,
        bitb_HL_ 1,
        halt
    ]

pr23 = [
        ldrn A 0x80,
        setbr 1 A,
        setbr 3 A,
        halt
    ]

pr24 = [
        ldrn A 0xff,
        resbr 1 A,
        resbr 3 A,
        halt
    ]

pr25 = [
        ldrn A 0x82,
        ld_nn_A 0x00ff,
        ldRRnn HL 0x00ff,
        setb_HL_ 2,
        ldA_nn_ 0x00ff,        
        setb_HL_ 7,
        ldA_nn_ 0x00ff,        
        setb_HL_ 6,
        ldA_nn_ 0x00ff,        
        setb_HL_ 0,
        ldA_nn_ 0x00ff,        
        halt
    ]

pr26 = [
        ldrn A 0xff,
        ld_nn_A 0x00ff,
        ldRRnn HL 0x00ff,
        resb_HL_ 2,
        ldA_nn_ 0x00ff,        
        resb_HL_ 7,
        ldA_nn_ 0x00ff,        
        resb_HL_ 6,
        ldA_nn_ 0x00ff,        
        resb_HL_ 0,
        ldA_nn_ 0x00ff,        
        halt
    ]

pr27 = [
        ldRRnn DE 0x00ff,
        incRR DE,
        ldRRnn DE 0x1000,
        decRR DE,
        halt
    ]

pr' = pr27

-- =============================================== --

flagMask_Z = 0x01
flagMask_INVERSEZ = 0xfe

type PCState = StateT MyState IO ()
type PCState_ = StateT MyState IO

data Registers = Registers {areg :: Int, breg :: Int, creg :: Int, dreg :: Int, ereg :: Int, freg :: Int, hreg :: Int, lreg :: Int, spreg :: Int}

instance Show Registers where
    show rs = "(REGISTERS: a = " ++ showHex (areg rs)
        ++ " bc = " ++ showHex16 (to16Bit (breg rs) (creg rs))
        ++ " de = " ++ showHex16 (to16Bit (dreg rs) (ereg rs))
        ++ " hl = " ++ showHex16 (to16Bit (hreg rs) (lreg rs))
        ++ " sp=" ++ showHex16 (spreg rs)
        ++ " f = " ++ showHex (freg rs) ++ " )"

to16Bit :: Int -> Int -> Int
to16Bit x y = ((x .&. 0xff) `shiftL` 8) .|. (y .&. 0xff)

to2x8 :: Int -> (Int, Int)
to2x8 v = ( ((v .&. 0xff00) `shiftR` 8) .&. 0xff, v .&. 0xff )

data IORegisters = IORegisters {ioControlA :: Int, ioValueA :: Int , ioControlB :: Int, ioValueB :: Int}
instance Show IORegisters where
    show iors = "(IOREGISTERS: ioControlA = "   ++ showHex (ioControlA iors) ++ ", "
                                                ++ "ioValueA = " ++ showHex (ioValueA iors) ++ ", "
                                                ++ "ioControlA = " ++ showHex (ioControlB iors) ++ ", "
                                                ++ "ioValueB = " ++ showHex (ioValueB iors) ++ ")"


data ProgramData = ProgramData {ins :: [RawInstruction], lab :: [(String, Int)]}
type Mem = H.Map Int Int
data MyState = MyState { canContinue :: Bool, programData :: ProgramData, pc  :: Int, registers :: Registers, ioRegisters :: IORegisters, memory :: Mem, e :: String }

type RawInstruction = (Int, PCState)
data MachineInstruction = Labeled String RawInstruction | Unlabled RawInstruction

data Address = LabelValue String | NumberValue Int deriving Show

data Label = Label String

addLabel :: Label -> MachineInstruction -> MachineInstruction
addLabel (Label label) (Unlabled instr) = Labeled label instr
addLabel _ instr = instr

(.>) :: Label -> MachineInstruction -> MachineInstruction
(.>) = addLabel

runMachine :: [MachineInstruction] -> IO ()
runMachine instructions  = let
            initRegisters = Registers {areg = 0, breg = 0, creg = 0, dreg = 0, ereg = 0, freg = 0, hreg = 0, lreg = 0, spreg = 0}
            initIORegisters = IORegisters {ioControlA = 0, ioValueA = 0, ioControlB = 0, ioValueB = 0}
            initMemory = H.fromList $ zip [0..0xffff] (repeat 0)

            seed :: [MachineInstruction] -> Int -> [RawInstruction] -> [(String, Int)] -> (Int, [RawInstruction], [(String, Int)])
            seed [] addr acc labelAcc  = (addr, acc, labelAcc)
            seed (Unlabled (a, i):is) addr acc labelAcc = seed is (addr+a)  ((addr, i):acc) labelAcc
            seed (Labeled labelName (a, i):is) addr acc labelAcc = seed is (addr+a)  ((addr, i):acc) ((labelName, addr):labelAcc)

            (topAddr, instructions', labels) = seed instructions 0 [] []

            initState = MyState True (ProgramData instructions' labels) 0 initRegisters initIORegisters initMemory ""

        in do
            putStrLn $ "top address:" ++ showHex16 topAddr
            putStrLn $ show labels
            putStrLn $ show initState
            (exitedWithError, newState) <- runStateT runMe initState
            putStrLn $ show exitedWithError

runMe :: PCState
runMe = do
    MyState _ instructions pc regs ioRegs mem e <- get
    let nextInstruction' = lookup pc (ins instructions)
    case nextInstruction' of
        Nothing -> return ()
        Just nextInstruction -> do
            nextInstruction
            newState <- get
            lift $ putStrLn $ show newState
            if (canContinue newState) then runMe else return ()

dodebug :: String -> PCState
dodebug s = do
        lift $ putStrLn s

debug :: String -> MachineInstruction -> MachineInstruction
debug d (Labeled s (n, p)) = Labeled s (n, dodebug d >>  p)
debug d (Unlabled (n, p)) = Unlabled (n, dodebug d >> p)

showHex :: Int -> String
showHex v
    | v >= 0 && v <= 255 = "0x" ++ (hexChar (  (v .&. 0xF0) `shiftR` 4  )) ++ (hexChar ( v .&. 0x0F ))
    | otherwise = "NUMBER OUT OF RANGE:"++ show v

showHex16 :: Int -> String
showHex16 v
    | v >= 0 && v <= 65535 = "0x" ++  (hexChar ( (v .&. 0xF000) `shiftR` 12 )) ++ (hexChar ( (v .&. 0x0F00) `shiftR` 8 )) ++ (hexChar ( (v .&. 0x00F0) `shiftR` 4 )) ++ (hexChar ( v .&. 0x000F ))
    | otherwise = "NUMBER OUT OF RANGE:"++ show v

hexChar val = [maybe '-' id (lookup val (zip [0..] "0123456789abcdef" ))]

--main :: IO ()
main = do

{-
00 is val a
01 is val b
02 is con a
03 is con b

LD A,h_4F;
OUT (h_02),A;
LD A,h_0F;
OUT (h_03),A;
LOOP1 : IN A,(h_00);
OUT (h_01),A;
JP LOOP1;
-}
        
    runMachine pr'

instance Show MyState where
    show (MyState cont i pc regs ioRegs mem e) = concat $ intersperse " " $ [
            "canContinue =", show cont,
            "pc =", showHex16 pc,
            "registers =", show regs,
            "ioRegisters =", show ioRegs,
            "e =", e
        ]

jp :: Address -> MachineInstruction
jp addr = Unlabled (3, do
    lift $ putStrLn ("jp " ++ show addr)
    MyState _ i pc regs ioRegs mem e <- get
    newPc <- decodeAddress addr

    case newPc of
        Nothing -> fatalError
        Just addr' -> put $ MyState True i addr' regs ioRegs mem e
 )

fatalError = do
    state <- get
    put $ state {canContinue = False, e = "ERROR"}

decodeAddress :: Address -> StateT MyState IO (Maybe Int)
decodeAddress (NumberValue num) = return $ Just num
decodeAddress (LabelValue label) = do
    state <- get
    return $ lookup label ((lab . programData) state)

ldA :: Int -> MachineInstruction
ldA num = Unlabled (2, do
        lift $ putStrLn ("ldA " ++ showHex num)
        MyState _ i pc regs ioRegs mem e <- get
        put $ MyState True i (pc + 2) (regs { areg = num}) ioRegs mem e
        )

halt :: MachineInstruction
halt = Unlabled (1, do
        lift $ putStrLn "halt"
        MyState _ i pc regs ioRegs mem e <- get
        let newPc = (pc + 1)
        put $ MyState False i newPc regs ioRegs mem "HALT"
        )

outA :: Int -> MachineInstruction
outA port = Unlabled (2,
        do
            lift $ putStrLn ("outA " ++ showHex port)
            state@(MyState cont i pc regs ioRegs mem e) <- get

            let
                newPc = (pc + 2)
                res = outState port regs ioRegs

            case res of
                Nothing -> put $ MyState False i newPc regs ioRegs mem "ERROR"
                Just (regs', ioRegs') -> do
                    put $ MyState True i newPc regs' ioRegs' mem e
                    outAction port (areg regs')

    )

inA :: Int -> MachineInstruction
inA port = Unlabled (2,
        do
            lift $ putStrLn ("inA " ++ showHex port)
            state@(MyState _ i pc regs ioRegs mem e) <- get
            res <- inAction port regs ioRegs

            case res of
                Nothing -> put $ MyState False i pc regs ioRegs mem "ERROR"
                Just (regs', inValue, ioRegs') -> put $ MyState True i (pc + 2) (regs {areg = inValue} ) ioRegs' mem e
    )

inAction 0x00 regs ioRegs | ioControlA ioRegs == 0x4f = do
    lift $ putStr "Please enter a value for port 0x00:"
    lift $ hFlush stdout
    newVal <- lift (readLn :: IO Int)
    return $ Just (regs, newVal, ioRegs { ioValueA = newVal } )

inAction  _ _ _ = return Nothing

{-
00 is val a
01 is val b
02 is con a
03 is con b
-}

outState :: Int -> Registers -> IORegisters -> Maybe (Registers, IORegisters)
outState port regs ioRegs
    | port == 0x01 && ioControlB ioRegs == 0x0f = Just (regs, ioRegs { ioValueB = areg regs })
    | port == 0x02 && areg regs == 0x4f = Just (regs, ioRegs { ioControlA = areg regs })
    | port == 0x03 && areg regs == 0x0f = Just (regs, ioRegs { ioControlB = areg regs })
outState _ _ _ = Nothing

outAction 0x00 a = lift $ putStrLn ("NEW PORT VALUE A: " ++ showHex a)
outAction 0x01 a = lift $ putStrLn ("NEW PORT VALUE B: " ++ showHex a)
outAction 0x02 a = lift $ putStrLn ("NEW PORT CONTROL A: " ++ showHex a)
outAction 0x03 a = lift $ putStrLn ("NEW PORT CONTROL B: " ++ showHex a)
outAction _ _ = return ()

cpA :: Int -> MachineInstruction
cpA val = Unlabled (2,
        do
            lift $ putStrLn ("cpA " ++ showHex val)
            state@(MyState _ i pc regs ioRegs mem e) <- get

            let
                newPc = (pc + 2)
                newFReg = if areg regs == val
                  then freg regs .|. flagMask_Z
                  else freg regs .&. flagMask_INVERSEZ

            put $ state {pc = newPc, registers = regs {freg = newFReg} }
    )

jpNZ :: Address -> MachineInstruction
jpNZ addr = Unlabled (2, do
        lift $ putStrLn ("jpNZ " ++ show addr)
        MyState _ i pc regs ioRegs mem e <- get

        if freg regs .&. flagMask_Z == 0
          then do
            newPc <- decodeAddress addr
            case newPc of
                Nothing -> fatalError
                Just addr' -> put $ MyState True i addr' regs ioRegs mem e
          else
            put $ MyState True i (pc + 2) regs ioRegs mem e
    )

jpZ :: Address -> MachineInstruction
jpZ addr = Unlabled (2, do
        lift $ putStrLn ("jpNZ " ++ show addr)
        MyState _ i pc regs ioRegs mem e <- get

        if freg regs .&. flagMask_Z == flagMask_Z
          then do
            newPc <- decodeAddress addr
            case newPc of
                Nothing -> fatalError
                Just addr' -> put $ MyState True i addr' regs ioRegs mem e
          else
            put $ MyState True i (pc + 2) regs ioRegs mem e
    )

nop :: MachineInstruction
nop = Unlabled (1, do
        lift $ putStrLn "nop"
        state <- get
        put $ state {pc = pc state + 1}
    )

data Reg8Spec = A | F | B | C | D | E | H | L | S | P deriving Show
data Reg16Spec = AF | BC | DE | HL | SP deriving Show

dumpMemory :: Int -> Mem -> PCState
dumpMemory upto mem = lift $ putStrLn $ show (take upto $ map (\(a,v) -> (showHex16 a, showHex v) )  (H.toList mem))

ldRRnn :: Reg16Spec -> Int -> MachineInstruction
ldRRnn reg16 num = Unlabled (3, do
    lift $ putStrLn $ "ldRRnn " ++ show reg16 ++ " " ++ showHex16 num
    _ldRRnn 3 reg16 num
 )

_ldRRnn :: Int -> Reg16Spec -> Int -> PCState
_ldRRnn size SP num = do
    state <- get
    let regs' = registers state
    put $ state {pc = pc state + size, registers = regs' {spreg = num} }

_ldRRnn size reg16 num = do
    state <- get
    let
        (rh, rl) = getReg16Pair reg16
        regs' = registers state
        low = num .&. 0x00ff
        high = ((num .&. 0xff00) `shiftR` 8) .&. 0xff
        rhigh = modifyReg rh (const high) regs'
        rlow = modifyReg rl (const low) rhigh
    put $ state {pc = pc state + size, registers = rlow}

ldRR_nn_ :: Reg16Spec -> Address -> MachineInstruction
ldRR_nn_ reg16 addr'' = Unlabled (4, do
    addr' <- decodeAddress addr''
    case addr' of
        Just addr -> do

            lift $ putStrLn $ "ldRR_nn_ " ++ show reg16 ++ " " ++ showHex16 addr
            state <- get
            let
                mem = memory state
                hmem' = H.lookup (addr + 1) mem
                lmem' = H.lookup addr mem
            case (hmem', lmem') of
                (Just hmem, Just lmem) -> do
                    _ldRRnn 4 reg16 (to16Bit hmem lmem)
                otherwise -> fatalError
        otherwise -> fatalError
     )

ld_nn_RR :: Address -> Reg16Spec -> MachineInstruction
ld_nn_RR addr HL = _ld_nn_RR addr HL 3
ld_nn_RR addr reg16 = _ld_nn_RR addr reg16 4

_ld_nn_RR :: Address -> Reg16Spec -> Int -> MachineInstruction
_ld_nn_RR addr'' reg16 size = Unlabled (size, do
    addr' <- decodeAddress addr''
    case addr' of
        Just addr -> do
            lift $ putStrLn $ "ld_nn_RR " ++ show reg16 ++ " " ++ showHex16 addr
            state <- get
            let
                mem = memory state
                regs = registers state
                (high, low) = getReg16Pair reg16
                hval = valueReg high regs
                lval = valueReg low regs

                mem1 = H.insert addr lval mem
                mem2 = H.insert (addr + 1) hval mem1

            put $ state {pc = pc state + size, memory = mem2}
        otherwise -> fatalError
 )

ld_nn_A :: Int -> MachineInstruction
ld_nn_A loc = Unlabled (3, do
    lift $ putStrLn $ "ld_nn_A " ++ showHex16 loc
    state <- get
    let
        mem = memory state
        regs = registers state
        mem' = H.insert loc (areg regs) mem
    put $ state {pc = pc state + 3, memory = mem'}
 )

ldA_nn_ :: Int -> MachineInstruction
ldA_nn_ loc = Unlabled (3, do
    lift $ putStrLn $ "ldA_nn_ " ++ showHex16 loc
    state <- get
    let
        mem = memory state
        val = H.lookup loc mem
        regs = registers state
    case val of
        Nothing -> put $ state {canContinue = False, e = "ERROR"}
        Just val' -> put $ state {pc = pc state + 3, registers = regs {areg = val'}}
 )

ldrr :: Reg8Spec -> Reg8Spec -> MachineInstruction
ldrr r r' = Unlabled (1, do
    lift $ putStrLn $ "ldrr " ++ show r ++ " " ++ show r'
    state <- get
    let
        regs = registers state
        v' = valueReg r' regs
        regs' = modifyReg r (const v') regs

    put $ state { pc = (pc state) + 1, registers = regs' }
 )

mop :: (Int -> Int -> Int) -> (Int -> Int -> PCState_ (Bool, Int))
mop f = (\x y -> return $ (True, f x y))

addAr :: Reg8Spec -> MachineInstruction
addAr reg8 = _regA_Arith_Ar 1 reg8 (mop (+)) "addAr "

addAn :: Int -> MachineInstruction
addAn num = _regA_Arith_An 2 num (mop (+)) "addAn "

addA_HL_ :: MachineInstruction
addA_HL_ = _regA_Arith_A_HL_ 1 (mop (+)) "addA_HL_ "

subAr :: Reg8Spec -> MachineInstruction
subAr reg8 = _regA_Arith_Ar 1 reg8 (mop (-)) "subAr "

subAn :: Int -> MachineInstruction
subAn num = _regA_Arith_An 2 num (mop (-)) "subAn "

subA_HL_ :: MachineInstruction
subA_HL_ = _regA_Arith_A_HL_ 1 (mop (-)) "subA_HL_ "

andAr :: Reg8Spec -> MachineInstruction
andAr reg8 = _regA_Arith_Ar 1 reg8 (mop (.&.)) "andAr "

andAn :: Int -> MachineInstruction
andAn num = _regA_Arith_An 2 num (mop (.&.)) "andAn "

andA_HL_ :: MachineInstruction
andA_HL_ = _regA_Arith_A_HL_ 1 (mop (.&.)) "andA_HL_ "

orAr :: Reg8Spec -> MachineInstruction
orAr reg8 = _regA_Arith_Ar 1 reg8 (mop (.|.)) "orAr "

orAn :: Int -> MachineInstruction
orAn num = _regA_Arith_An 2 num (mop (.|.)) "orAn "

orA_HL_ :: MachineInstruction
orA_HL_ = _regA_Arith_A_HL_ 1 (mop (.|.)) "orA_HL_ "

xorAr :: Reg8Spec -> MachineInstruction
xorAr reg8 = _regA_Arith_Ar 1 reg8 (mop xor) "xorAr "

xorAn :: Int -> MachineInstruction
xorAn num = _regA_Arith_An 2 num (mop xor) "xorAn "

xorA_HL_ :: MachineInstruction
xorA_HL_ = _regA_Arith_A_HL_ 1 (mop xor) "xorA_HL_ "

cpAction :: Int -> Int -> PCState_ (Bool, Int)
cpAction r v = do
    updateZeroFlag' (r == v)
    return (False, r)

cpr :: Reg8Spec -> MachineInstruction
cpr reg8 = _regA_Arith_Ar 1 reg8 cpAction "cpr "

cpn :: Int -> MachineInstruction
cpn num = _regA_Arith_An 2 num cpAction "cpAn "

cp_HL_ :: MachineInstruction
cp_HL_ = _regA_Arith_A_HL_ 1 cpAction "cpA_HL "

updateZeroFlag :: PCState
updateZeroFlag = do
    state <- get
    let
        regs = registers state
        aval = valueReg A regs
        newFReg = if aval == 0
            then freg regs .|. flagMask_Z
            else freg regs .&. flagMask_INVERSEZ
        regs' = modifyReg F (const newFReg) regs
    put $ state { registers = regs' }

updateZeroFlag' :: Bool -> PCState
updateZeroFlag' cond = do
    state <- get
    let
        regs = registers state
        newFReg = if cond
            then freg regs .|. flagMask_Z
            else freg regs .&. flagMask_INVERSEZ
        regs' = modifyReg F (const newFReg) regs
    put $ state { registers = regs' }


_regA_Arith_Ar :: Int -> Reg8Spec -> (Int -> Int -> PCState_ (Bool, Int)) -> String -> MachineInstruction
_regA_Arith_Ar size reg8 _op name = Unlabled (size, do
    lift $ putStrLn $ name ++ show reg8
    state <- get
    let
        regs = registers state
        v' = valueReg reg8 regs
    modifyRegM A (`_op` v')

    state' <- get
    put $ state' { pc = (pc state') + size}
 )

_regA_Arith_An :: Int -> Int -> (Int -> Int -> PCState_ (Bool, Int)) -> String -> MachineInstruction
_regA_Arith_An size num _op name = Unlabled (size, do
    lift $ putStrLn $ name ++ showHex num
    modifyRegM A (`_op` num)

    state <- get
    put $ state { pc = (pc state) + size}
 )

_regA_Arith_A_HL_ :: Int -> (Int -> Int -> PCState_ (Bool, Int)) -> String -> MachineInstruction
_regA_Arith_A_HL_ size _op name = Unlabled (size, do
    lift $ putStrLn $ name ++ "HL"
    state <- get
    let
        mem = memory state
        regs = registers state
        addrh = valueReg H regs
        addrl = valueReg L regs

        addr = to16Bit addrh addrl
        memval' = H.lookup addr mem

    case memval' of
        Just memval -> do
            modifyRegM A (`_op` memval)
            state' <- get
            put $ state' { pc = (pc state') + size }
        otherwise -> fatalError
 )

incr :: Reg8Spec -> MachineInstruction
incr reg8 = _adjr reg8 (\v -> v + 1) "inc "

decr :: Reg8Spec -> MachineInstruction
decr reg8 = _adjr reg8 (\v -> v - 1) "dec "

_adjr :: Reg8Spec -> (Int -> Int) -> String -> MachineInstruction
_adjr reg8 adj name = Unlabled (1, do
    lift $ putStrLn $ name ++ (show reg8)
    state <- get
    let
        regs = registers state
        val = valueReg reg8 regs
        newval = adj val
        newRegs = modifyReg reg8 (const newval) regs
    put $ state { pc = (pc state) + 1, registers = newRegs }

    updateZeroFlag' ((newval .&. 0xff) == 0)
 )

inc_HL_ :: MachineInstruction
inc_HL_ = _adj_HL_ (\v -> v + 1) "inc "

dec_HL_ :: MachineInstruction
dec_HL_ = _adj_HL_ (\v -> v - 1) "dec "

_adj_HL_ :: (Int -> Int) -> String -> MachineInstruction
_adj_HL_ adj name = Unlabled (1, do
    lift $ putStrLn $ name ++ "(HL)"
    state <- get
    let
        mem = memory state
        regs = registers state
        addrh = valueReg H regs
        addrl = valueReg L regs

        addr = to16Bit addrh addrl
        memval' = H.lookup addr mem

    case memval' of
        Just memval -> do
            let
                newval = adj memval
                mem' = H.insert addr newval mem
            put $ state { pc = (pc state) + 1, memory = mem' }
            updateZeroFlag' ((newval .&. 0xff) == 0)
        otherwise -> fatalError
 )

rrca :: MachineInstruction
rrca = Unlabled (1, do
    lift $ putStrLn "rrca"
    state <- get
    let
        regs = registers state
        aval = valueReg A regs
        bit0 = aval .&. 0x01
        arot = (rotateR aval 1) .&. 0xff
        aval' = if bit0 > 0 then arot .|. 0x80 else arot
        regs' = modifyReg A (const aval') regs
    put $ state { pc = (pc state) + 1, registers = regs' }
 )

rlca :: MachineInstruction
rlca = Unlabled (1, do
    lift $ putStrLn "rlca"
    state <- get
    let
        regs = registers state
        aval = valueReg A regs
        bit7 = aval .&. 0x80
        arot = (rotateL aval 1) .&. 0xff
        aval' = if bit7 > 0 then arot .|. 0x01 else arot
        regs' = modifyReg A (const aval') regs
    put $ state { pc = (pc state) + 1, registers = regs' }
 )

bitbr :: Int -> Reg8Spec -> MachineInstruction
bitbr bit reg8 = Unlabled (2, do
    lift $ putStrLn $ "bitbr " ++ showHex bit ++ " " ++ show reg8
    state <- get
    let
        regs = registers state
        bitMask = 2 ^ bit
        rval = valueReg reg8 regs
        bitVal = rval .&. bitMask
    updateZeroFlag' (bitVal == 0)
    incPC 2
 )

bitb_HL_ :: Int -> MachineInstruction
bitb_HL_ bit = Unlabled (2, do
    lift $ putStrLn $ "bitb_HL_ " ++ showHex bit
    state <- get
    let
        mem = memory state
        regs = registers state
        bitMask = 2 ^ bit

        addrh = valueReg H regs
        addrl = valueReg L regs

        addr = to16Bit addrh addrl
        memval' = H.lookup addr mem

    case memval' of
        Just memval -> do
            let bitVal = memval .&. bitMask
            updateZeroFlag' (bitVal == 0)
            incPC 2
        otherwise -> fatalError
 )

setbr :: Int -> Reg8Spec -> MachineInstruction
setbr bit reg8 = Unlabled (2, do
    _modifybr bit reg8 True 2 "setbr "
 )

resbr :: Int -> Reg8Spec -> MachineInstruction
resbr bit reg8 = Unlabled (2, do
    _modifybr bit reg8 False 2 "resbr "
 )

_modifybr :: Int -> Reg8Spec -> Bool -> Int -> String -> PCState
_modifybr bit reg8 val size name = do
    lift $ putStrLn $ name ++ showHex bit ++ " " ++ show reg8
    state <- get
    let
        regs = registers state
        rval = valueReg reg8 regs
        bitMask = 2 ^ bit
        bitMaskInverse = (complement  (2 ^ bit) ) .&. 0xff
        newrval = if val then rval .|. bitMask else rval .&. bitMaskInverse
        regs' = modifyReg reg8 (const newrval) regs
    put $ state {registers = regs'}
    incPC size

setb_HL_ :: Int -> MachineInstruction
setb_HL_ bit = Unlabled (2, do
    _modifyb_HL_ bit True 2 "setb_HL_ "
 )

resb_HL_ :: Int -> MachineInstruction
resb_HL_ bit = Unlabled (2, do
    _modifyb_HL_ bit False 2 "resb_HL_ "
 )

_modifyb_HL_ :: Int -> Bool -> Int -> String -> PCState
_modifyb_HL_ bit val size name = do
    lift $ putStrLn $ name ++ showHex bit
    state <- get
    let
        regs = registers state
        mem = memory state

        addrh = valueReg H regs
        addrl = valueReg L regs

        addr = to16Bit addrh addrl
        memval' = H.lookup addr mem

    case memval' of
        Just memval -> do
            let
                bitMask = 2 ^ bit
                bitMaskInverse = (complement  (2 ^ bit) ) .&. 0xff
                newmemval = if val then memval .|. bitMask else memval .&. bitMaskInverse
                mem' = H.insert addr newmemval mem
            put $ state {memory = mem'}
            incPC size
        otherwise -> fatalError

incRR :: Reg16Spec -> MachineInstruction
incRR reg16 = Unlabled (1, do
    _adjRR reg16 (\v -> v + 1) 1 "incRR"
 )

decRR :: Reg16Spec -> MachineInstruction
decRR reg16 = Unlabled (1, do
    _adjRR reg16 (\v -> v - 1) 1 "decRR"
 )

_adjRR :: Reg16Spec -> (Int -> Int) -> Int -> String -> PCState
_adjRR AF _ _ _ = fatalError
_adjRR reg16 _op size name = do
    lift $ putStrLn $ name ++ " " ++ show reg16
    state <- get
    let
        (rh, rl) = getReg16Pair reg16
        regs = registers state
        hval = valueReg rh regs
        lval = valueReg rl regs
        rrval = to16Bit hval lval
        newrrval = (_op rrval) .&. 0xffff

        (newhval, newlval) = to2x8 newrrval
        regs1 = modifyReg rh (const newhval) regs
        regs2 = modifyReg rl (const newlval) regs1
    put $ state {registers = regs2}
    incPC size

di :: MachineInstruction
di = Unlabled (1, do
    incPC 1
 )

incPC :: Int -> PCState
incPC num = do
    state <- get
    put $ state { pc = (pc state) + num}

ldrn :: Reg8Spec -> Int -> MachineInstruction
ldrn reg8 num = Unlabled (2, do
    lift $ putStrLn $ "ldrn " ++ show reg8 ++ " " ++ showHex num
    state <- get
    let
        regs = registers state
        regs' = modifyReg reg8 (const num) regs
    put $ state { pc = (pc state) + 2, registers = regs' }
 )

push :: Reg16Spec -> MachineInstruction
push reg16 = Unlabled (1, do
    lift $ putStrLn $ "push " ++ show reg16
    state <- get
    let
        regs = registers state
        (high, low) = getReg16Pair reg16
        hval = valueReg high regs
        lval = valueReg low regs
    _pushValue (hval, lval)
    state' <- get
    put $ state' {pc = (pc state') + 1}
 )

pop :: Reg16Spec -> MachineInstruction
pop reg16 = Unlabled (1, do
    lift $ putStrLn $ "pop " ++ show reg16

    state <- get
    let
        (high, low) = getReg16Pair reg16
        regs = registers state

    vals <- _popValue    
    case vals of
        Just (memhval,memlval) -> do
            let
                regs1 = modifyReg high (const memhval) regs
                regs2 = modifyReg low (const memlval) regs1
            put $ state {pc = (pc state) + 1, registers = regs2}
        otherwise -> put $ state {canContinue = False, e = "ERROR"}
 )

_popValue :: PCState_ (Maybe (Int, Int))
_popValue = do
    state <- get
    let
        regs = registers state
        mem = memory state
        sp = spreg regs
        memlval' = H.lookup sp mem
        memhval' = H.lookup (sp + 1) mem
    case (memhval', memlval') of
        (Just memhval, Just memlval) -> do
            put $ state {registers = regs {spreg = sp + 2}}
            return $ Just (memhval, memlval)
        otherwise -> return Nothing

_pushValue :: (Int, Int) -> PCState
_pushValue (hval, lval) = do
    state <- get
    let
        regs = registers state
        sp = spreg regs
        mem = memory state
        mem1 = H.insert (sp - 2) lval mem
        mem2 = H.insert (sp - 1) hval mem1
    put $ state {registers = regs {spreg = sp - 2}, memory = mem2}

call :: Address -> MachineInstruction
call addr = Unlabled (3, do
    lift $ putStrLn $ "call " ++ show addr
    newPc' <- decodeAddress addr

    case newPc' of
        Just newPc -> do 
            state' <- get
            _pushValue $ to2x8 (pc state' + 3)
            state <- get
            put $ state {pc = newPc}
        otherwise -> fatalError
 )

ret :: MachineInstruction
ret = Unlabled (1, do
    lift $ putStrLn "ret"
    val <- _popValue
    case val of
        Just (high, low) -> do
            state <- get
            put $ state {pc = to16Bit high low}
        otherwise -> fatalError
 )

modifyReg :: Reg8Spec -> (Int -> Int) -> Registers -> Registers
modifyReg A f s = s { areg = f (areg s) .&. 0xff}
modifyReg F f s = s { freg = f (freg s) .&. 0xff}
modifyReg B f s = s { breg = f (breg s) .&. 0xff}
modifyReg C f s = s { creg = f (creg s) .&. 0xff}
modifyReg D f s = s { dreg = f (dreg s) .&. 0xff}
modifyReg E f s = s { ereg = f (ereg s) .&. 0xff}
modifyReg H f s = s { hreg = f (hreg s) .&. 0xff}
modifyReg L f s = s { lreg = f (lreg s) .&. 0xff}

modifyRegM :: Reg8Spec -> (Int -> PCState_ (Bool, Int)) -> PCState
modifyRegM reg8 f = do
    state <- get
    let
        regs = registers state
    (shouldUpdate, value) <- f (valueReg reg8 regs)
    if shouldUpdate
      then do
        state' <- get
        let newRegs = modifyReg reg8 (const value) (registers state')
        put $ state' {registers = newRegs}
        updateZeroFlag
      else
        return ()

valueReg :: Reg8Spec -> Registers -> Int
valueReg A regs = areg regs
valueReg F regs = freg regs
valueReg B regs = breg regs
valueReg C regs = creg regs
valueReg D regs = dreg regs
valueReg E regs = ereg regs
valueReg H regs = hreg regs
valueReg L regs = lreg regs

getReg16Pair :: Reg16Spec -> (Reg8Spec, Reg8Spec)
getReg16Pair AF = (A, F)
getReg16Pair BC = (B, C)
getReg16Pair DE = (D, E)
getReg16Pair HL = (H, L)
          
{-
LD A,h_4F;
OUT (h_02),A;
LD A,h_0F;
OUT (h_03),A;
LOOP1 : IN A,(h_00);
OUT (h_01),A;
JP LOOP1;
-}
