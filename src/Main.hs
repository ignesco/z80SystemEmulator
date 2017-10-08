module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import qualified Data.HashMap as H

import Data.Bits
import Data.List
import System.IO


pr1 = [halt]

prt1 = [
        Unlabled $ ldA 0x34,
        Unlabled $ cpA 0x34,
        Unlabled $ cpA 0x88,
        Unlabled $ jpNZ (NumberValue 0x0000),
        Unlabled $ halt
        ]

pr2 = [

            Label "START" $     ldA 0x4F,
            Unlabled $          outA 0x02,

            Unlabled $          ldA 0x0F,
            Unlabled $          outA 0x03,
            Label "LOOP1" $     inA 0x00,
            Unlabled $          outA 0x01,

            Unlabled $          cpA 0x00,

            Unlabled $          jpNZ $ LabelValue "LOOP1",

            Unlabled $ nop,
            Label "END" $ halt
            ]

pr3 = [
            Unlabled $ ldRRnn BC 0x5499,
            Unlabled $ ldRRnn HL 0x1234,
            Unlabled $ ldRRnn SP 0xfedc,
            Unlabled $ ldA 0xff,
            Unlabled $ ld_nn_A 0x3265,
            Unlabled $ ldA 0xee,
            Unlabled $ ld_nn_A 0x3266,
            Unlabled $ ldA_nn_ 0x3265,
            Unlabled $ ldA_nn_ 0x3266,

            Unlabled halt
            ]

pr4 = [
        Unlabled $ ldRRnn SP 0x00ff,
        Unlabled $ ldRRnn HL 0x1234,
        Unlabled $ push HL,
        Unlabled $ ldRRnn HL 0x0000,
        Unlabled $ pop DE,
        Unlabled $ halt
    ]

pr5 = [
        Unlabled $ ldRRnn SP 0x00ff,
        Unlabled $ call $ LabelValue "TheSub",
        Unlabled $ nop,
        Unlabled $ nop,
        Unlabled $ nop,
        Unlabled $ nop,
        Unlabled $ halt,

        Label "TheSub" $ nop,
        Unlabled $ nop,
        Unlabled $ nop,
        Unlabled $ ret
    ]

pr6 = [
        Unlabled $ ldRRnn SP 0x00ff,
        Unlabled $ ldRRnn HL 0x1234,
        Unlabled $ push HL,
        Unlabled $ ldRR_nn_ DE 0x00fd,
        Unlabled $ pop BC,
        Unlabled $ halt        
    ]

pr' = pr6


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
        ++ " sp=" ++ showHex16 (spreg rs) ++ " )"
        ++ " f = " ++ showHex (freg rs)

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


data ProgramData = ProgramData {ins :: [(Int, PCState)], lab :: [(String, Int)]}
type Mem = H.Map Int Int
data MyState = MyState { canContinue :: Bool, programData :: ProgramData, pc  :: Int, registers :: Registers, ioRegisters :: IORegisters, memory :: Mem, e :: String }

data AnnotatedPCState = Label String (Int, PCState) | Unlabled (Int, PCState)

data Address = LabelValue String | NumberValue Int deriving Show

runMachine :: [AnnotatedPCState] -> IO ()
runMachine instructions  = let
            initRegisters = Registers {areg = 0, breg = 0, creg = 0, dreg = 0, ereg = 0, freg = 0, hreg = 0, lreg = 0, spreg = 0}
            initIORegisters = IORegisters {ioControlA = 0, ioValueA = 0, ioControlB = 0, ioValueB = 0}
            initMemory = H.fromList $ zip [0..0xffff] (repeat 0)

            seed :: [AnnotatedPCState] -> Int -> [(Int, PCState)] -> [(String, Int)] -> (Int, [(Int, PCState)], [(String, Int)])
            seed [] addr acc labelAcc  = (addr, acc, labelAcc)
            seed (Unlabled (a, i):is) addr acc labelAcc = seed is (addr+a)  ((addr, i):acc) labelAcc
            seed (Label labelName (a, i):is) addr acc labelAcc = seed is (addr+a)  ((addr, i):acc) ((labelName, addr):labelAcc)

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

debug :: String -> AnnotatedPCState -> AnnotatedPCState
debug d (Label s (n, p)) = Label s (n, dodebug d >>  p)
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

jp :: Address -> (Int, PCState)
jp addr = (3, do
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

ldA :: Int -> (Int, PCState)
ldA num = (2, do
        lift $ putStrLn ("ldA " ++ showHex num)
        MyState _ i pc regs ioRegs mem e <- get
        put $ MyState True i (pc + 2) (regs { areg = num}) ioRegs mem e
        )

halt :: (Int, PCState)
halt = (1, do
        lift $ putStrLn "halt"
        MyState _ i pc regs ioRegs mem e <- get
        let newPc = (pc + 1)
        put $ MyState False i newPc regs ioRegs mem "HALT"
        )

outA :: Int -> (Int, PCState)
outA port = (2,
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

inA :: Int -> (Int, PCState)
inA port = (2,
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

cpA :: Int -> (Int, PCState)
cpA val = (2,
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

jpNZ :: Address -> (Int, PCState)
jpNZ addr = (2, do
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

jpZ :: Address -> (Int, PCState)
jpZ addr = (2, do
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

nop :: (Int, PCState)
nop = (1, do
        lift $ putStrLn "nop"
        state <- get
        put $ state {pc = pc state + 1}
    )

data Reg8Spec = A | F | B | C | D | E | H | L | S | P deriving Show
data Reg16Spec = AF | BC | DE | HL | SP deriving Show


dumpMemory :: Int -> Mem -> PCState
dumpMemory upto mem = lift $ putStrLn $ show (take upto $ map (\(a,v) -> (showHex16 a, showHex v) )  (H.toList mem))

ldRRnn :: Reg16Spec -> Int -> (Int, PCState)
ldRRnn reg16 num = (3, do
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

ldRR_nn_ :: Reg16Spec -> Int -> (Int, PCState)
ldRR_nn_ reg16 addr = (4, do
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
 )

ld_nn_A :: Int -> (Int, PCState)
ld_nn_A loc = (3, do
    lift $ putStrLn $ "ld_nn_A " ++ showHex16 loc
    state <- get
    let
        mem = memory state
        regs = registers state
        mem' = H.insert loc (areg regs) mem
    put $ state {pc = pc state + 3, memory = mem'}
 )

ldA_nn_ :: Int -> (Int, PCState)
ldA_nn_ loc = (3, do
    lift $ putStrLn $ "ldA_nn_" ++ showHex16 loc
    state <- get
    let
        mem = memory state
        val = H.lookup loc mem
        regs = registers state
    case val of
        Nothing -> put $ state {canContinue = False, e = "ERROR"}
        Just val' -> put $ state {pc = pc state + 3, registers = regs {areg = val'}}
 )

push :: Reg16Spec -> (Int, PCState)
push reg16 = (1, do
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

pop :: Reg16Spec -> (Int, PCState)
pop reg16 = (1, do
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

call :: Address -> (Int, PCState)
call addr = (3, do
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

ret :: (Int, PCState)
ret = (1, do
    lift $ putStrLn "ret"
    val <- _popValue
    case val of
        Just (high, low) -> do
            state <- get
            put $ state {pc = to16Bit high low}
        otherwise -> fatalError
 )

modifyReg :: Reg8Spec -> (Int -> Int) -> Registers -> Registers
modifyReg A f s = s { areg = f (areg s)}
modifyReg F f s = s { freg = f (freg s)}
modifyReg B f s = s { breg = f (breg s)}
modifyReg C f s = s { creg = f (creg s)}
modifyReg D f s = s { dreg = f (dreg s)}
modifyReg E f s = s { ereg = f (ereg s)}
modifyReg H f s = s { hreg = f (hreg s)}
modifyReg L f s = s { lreg = f (lreg s)}

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
