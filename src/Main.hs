module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import Data.Bits
import Data.List
import System.IO

flagMask_Z = 0x01
flagMask_INVERSEZ = 0xfe

type PCState = StateT MyState IO ()

data Registers = Registers {areg :: Int, breg :: Int, creg :: Int, dreg :: Int, ereg :: Int, freg :: Int, hreg :: Int, regl :: Int}

instance Show Registers where
    show rs = "(REGISTERS: a = " ++ showHex (areg rs) ++ " f = " ++ showHex (freg rs) ++ " )"

data IORegisters = IORegisters {ioControlA :: Int, ioValueA :: Int , ioControlB :: Int, ioValueB :: Int}
instance Show IORegisters where
    show iors = "(IOREGISTERS: ioControlA = "   ++ showHex (ioControlA iors) ++ ", "
                                                ++ "ioValueA = " ++ showHex (ioValueA iors) ++ ", "
                                                ++ "ioControlA = " ++ showHex (ioControlB iors) ++ ", "
                                                ++ "ioValueB = " ++ showHex (ioValueB iors) ++ ")"


data ProgramData = ProgramData {ins :: [(Int, PCState)], lab :: [(String, Int)]}
        
data MyState = MyState { canContinue :: Bool, programData :: ProgramData, pc  :: Int, registers :: Registers, ioRegisters :: IORegisters, e :: String }

data AnnotatedPCState = Label String (Int, PCState) | Unlabled (Int, PCState)

data Address = LabelValue String | NumberValue Int deriving Show

runMachine :: [AnnotatedPCState] -> IO ()
runMachine instructions  = let
            initRegisters = Registers {areg = 0, breg = 0, creg = 0, dreg = 0, ereg = 0, freg = 0, hreg = 0, regl = 0}
            initIORegisters = IORegisters {ioControlA = 0, ioValueA = 0, ioControlB = 0, ioValueB = 0}

            seed :: [AnnotatedPCState] -> Int -> [(Int, PCState)] -> [(String, Int)] -> ([(Int, PCState)], [(String, Int)])
            seed [] addr acc labelAcc  = (acc, labelAcc)
            seed (Unlabled (a, i):is) addr acc labelAcc = seed is (addr+a)  ((addr, i):acc) labelAcc
            seed (Label labelName (a, i):is) addr acc labelAcc = seed is (addr+a)  ((addr, i):acc) ((labelName, addr):labelAcc)

            (instructions', labels) = seed instructions 0 [] []


            initState = MyState True (ProgramData instructions' labels) 0 initRegisters initIORegisters ""

        in do
            putStrLn $ show labels
            putStrLn $ show initState
            (exitedWithError, newState) <- runStateT runMe initState
            putStrLn $ show exitedWithError

runMe :: PCState
runMe = do
    MyState _ instructions pc regs ioRegs e <- get
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

debug :: String -> (Int, PCState) -> (Int, PCState)
debug s (x, instruction) = (x, dodebug s >> instruction)

showHex :: Int -> String
showHex v
    | v >= 0 && v <= 255 = "0x" ++ (hexChar (  (v .&. 0xF0) `shiftR` 4  )) ++ (hexChar ( v .&. 0x0F ))
    | otherwise = "NUMBER OUT OF RANGE"

showHex16 :: Int -> String
showHex16 v
    | v >= 0 && v <= 65535 = "0x" ++  (hexChar ( (v .&. 0xF000) `shiftR` 12 )) ++ (hexChar ( (v .&. 0x0F00) `shiftR` 8 )) ++ (hexChar ( (v .&. 0x00F0) `shiftR` 4 )) ++ (hexChar ( v .&. 0x000F ))
    | otherwise = "NUMBER OUT OF RANGE"

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
        
    let pr1 = [halt]

    let prt1 = [
            Unlabled $ ldA 0x34,
            Unlabled $ cpA 0x34,
            Unlabled $ cpA 0x88,
            Unlabled $ jpNZ (NumberValue 0x00),
            Unlabled $ halt
            ]

    let pr2 = [

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
    runMachine pr2

instance Show MyState where
    show (MyState cont i pc regs ioRegs e) = concat $ intersperse " " $ [
            "canContinue =", show cont,
            "pc =", showHex16 pc,
            "registers =", show regs,
            "ioRegisters =", show ioRegs,
            "e =", e
        ]

jp :: Address -> (Int, PCState)
jp addr = (3, do
        lift $ putStrLn ("jp " ++ show addr)
        MyState _ i pc regs ioRegs e <- get
        newPc <- decodeAddress addr

        case newPc of
            Nothing -> fatalError
            Just addr' -> put $ MyState True i addr' regs ioRegs e
        )

fatalError = return ()

decodeAddress :: Address -> StateT MyState IO (Maybe Int)
decodeAddress (NumberValue num) = return $ Just num
decodeAddress (LabelValue label) = do
    state <- get
    return $ lookup label ((lab . programData) state)

ldA :: Int -> (Int, PCState)
ldA num = (2, do
        lift $ putStrLn ("ldA " ++ showHex num)
        MyState _ i pc regs ioRegs e <- get
        put $ MyState True i (pc + 2) (regs { areg = num}) ioRegs e
        )

halt :: (Int, PCState)
halt = (1, do
        lift $ putStrLn "halt"
        MyState _ i pc regs ioRegs e <- get
        let newPc = (pc + 1)
        put $ MyState False i newPc regs ioRegs "HALT"
        )

outA :: Int -> (Int, PCState)
outA port = (2,
        do
            lift $ putStrLn ("outA " ++ showHex port)
            state@(MyState cont i pc regs ioRegs e) <- get

            let
                newPc = (pc + 2)
                res = outState port regs ioRegs

            case res of
                Nothing -> put $ MyState False i newPc regs ioRegs "ERROR"
                Just (regs', ioRegs') -> do
                    put $ MyState True i newPc regs' ioRegs' e
                    outAction port (areg regs')

    )

inA :: Int -> (Int, PCState)
inA port = (2,
        do
            lift $ putStrLn ("inA " ++ showHex port)
            state@(MyState _ i pc regs ioRegs e) <- get
            res <- inAction port regs ioRegs

            case res of
                Nothing -> put $ MyState False i pc regs ioRegs "ERROR"
                Just (regs', inValue, ioRegs') -> put $ MyState True i (pc + 2) (regs {areg = inValue} ) ioRegs' e
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
            state@(MyState _ i pc regs ioRegs e) <- get

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
        MyState _ i pc regs ioRegs e <- get

        if freg regs .&. flagMask_Z == 0
          then do
            newPc <- decodeAddress addr
            case newPc of
                Nothing -> fatalError
                Just addr' -> put $ MyState True i addr' regs ioRegs e
          else
            put $ MyState True i (pc + 2) regs ioRegs e
    )

jpZ :: Address -> (Int, PCState)
jpZ addr = (2, do
        lift $ putStrLn ("jpNZ " ++ show addr)
        MyState _ i pc regs ioRegs e <- get

        if freg regs .&. flagMask_Z == flagMask_Z
          then do
            newPc <- decodeAddress addr
            case newPc of
                Nothing -> fatalError
                Just addr' -> put $ MyState True i addr' regs ioRegs e
          else
            put $ MyState True i (pc + 2) regs ioRegs e
    )

nop :: (Int, PCState)
nop = (1, do
        lift $ putStrLn "nop"
        state <- get
        put $ state {pc = pc state + 1}
    )

          
{-
LD A,h_4F;
OUT (h_02),A;
LD A,h_0F;
OUT (h_03),A;
LOOP1 : IN A,(h_00);
OUT (h_01),A;
JP LOOP1;
-}
