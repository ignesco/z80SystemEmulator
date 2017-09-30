module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import Data.Bits
import Data.List
import System.IO

flagMask_Z = 0x01

type PCState = StateT MyState IO ()

data Registers = Registers {areg :: Int, breg :: Int, creg :: Int, dreg :: Int, ereg :: Int, freg :: Int, hreg :: Int, regl :: Int}

instance Show Registers where
    show rs = "(REGISTERS: a = " ++ showHex (areg rs) ++ " f = " ++ showHex (freg rs) ++ " )"

data ProgramData = ProgramData {ins :: [(Int, PCState)], lab :: [(String, Int)]}
        
data MyState = MyState { canContinue :: Bool, programData :: ProgramData, pc  :: Int, registers :: Registers, ioControlA :: Int, ioAValue :: Int , ioControlB :: Int, ioBValue :: Int, e :: String }

data AnnotatedPCState = Label String (Int, PCState) | Unlabled (Int, PCState)

data Address = LabelValue String | NumberValue Int deriving Show

runMachine :: [AnnotatedPCState] -> IO ()
runMachine instructions  = let
            initRegisters = Registers {areg = 0, breg = 0, creg = 0, dreg = 0, ereg = 0, freg = 0, hreg = 0, regl = 0}

            seed :: [AnnotatedPCState] -> Int -> [(Int, PCState)] -> [(String, Int)] -> ([(Int, PCState)], [(String, Int)])
            seed [] addr acc labelAcc  = (acc, labelAcc)
            seed (Unlabled (a, i):is) addr acc labelAcc = seed is (addr+a)  ((addr, i):acc) labelAcc
            seed (Label labelName (a, i):is) addr acc labelAcc = seed is (addr+a)  ((addr, i):acc) ((labelName, addr):labelAcc)

            (instructions', labels) = seed instructions 0 [] []


            initState = MyState True (ProgramData instructions' labels) 0 initRegisters 0 0 0 0 ""

        in do
            putStrLn $ show labels
            putStrLn $ show initState
            (exitedWithError, newState) <- runStateT runMe initState
            putStrLn $ show exitedWithError

runMe :: PCState
runMe = do
    MyState _ instructions pc a ioCA ioVA ioCB ioVB e <- get
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

    let pr2 = [

                Label "START" $     ldA 0x4F,                   -- 0000
                Unlabled $          outA 0x02,                  -- 0002

                Unlabled $          ldA 0x0F,                   -- 0004
                Unlabled $          outA 0x03,                  -- 0006
                Label "LOOP1" $     inA 0x00,                   -- 0008
                Unlabled $          outA 0x01,                  -- 000a

                Unlabled $          jp $ LabelValue "LOOP1",    -- 000c

                Label "END" $ halt                              -- 000f
                ]
    runMachine pr2

instance Show MyState where
    show (MyState cont i pc regs ioCA ioVA ioCB ioVB e) = concat $ intersperse " " $ [
            "canContinue =", show cont,
            "pc =", showHex16 pc,
            "registers =", show regs,
            "ioCA =", showHex ioCA,
            "ioVA =", showHex ioVA,
            "ioCB =", showHex ioCB,
            "ioVB =", showHex ioVB,
            "e =", e
        ]

jp :: Address -> (Int, PCState)
jp addr = (3, do
        MyState _ i pc a ioCA ioVA ioCB ioVB e <- get
        newPc <- decodeAddress addr

        case newPc of
            Nothing -> fatalError
            Just addr' -> put $ MyState True i addr' a ioCA ioVA ioCB ioVB e
        lift $ putStrLn ("jp " ++ show addr)
        )

fatalError = return ()

decodeAddress :: Address -> StateT MyState IO (Maybe Int)
decodeAddress (NumberValue num) = return $ Just num
decodeAddress (LabelValue label) = do
    state <- get
    return $ lookup label ((lab . programData) state)

ldA :: Int -> (Int, PCState)
ldA num = (2, do
        MyState _ i pc regs ioCA ioVA ioCB ioVB e <- get
        let newPc = (pc + 2)
        put $ MyState True i newPc (regs { areg = num}) ioCA ioVA ioCB ioVB e
        lift $ putStrLn ("ldA " ++ showHex num)
        )

halt :: (Int, PCState)
halt = (1, do
        MyState _ i pc a ioCA ioVA ioCB ioVB e <- get
        let newPc = (pc + 1)
        put $ MyState False i newPc a ioCA ioVA ioCB ioVB "HALT"
        lift $ putStrLn "halt"
        )

outA :: Int -> (Int, PCState)
outA port = (2,
        do
            state@(MyState cont i pc regs ioCA ioVA ioCB ioVB e) <- get
            let
                newPc = (pc + 2)
                res = outState port regs ioCA ioVA ioCB ioVB

            case res of
                Nothing -> put $ MyState False i newPc regs ioCA ioVA ioCB ioVB "ERROR"
                Just (regs', ioCA', ioVA', ioCB', ioVB') -> do
                    put $ MyState True i newPc regs' ioCA' ioVA' ioCB' ioVB' e
                    outAction port (areg regs')

            lift $ putStrLn ("outA " ++ showHex port)
    )

inA :: Int -> (Int, PCState)
inA port = (2,
        do
            state@(MyState _ i pc regs ioCA ioVA ioCB ioVB e) <- get
            let
                newPc = (pc + 2)

            res <- inAction port regs ioCA ioVA ioCB ioVB

            case res of
                Nothing -> put $ MyState False i pc regs ioCA ioVA ioCB ioVB "ERROR"
                Just (regs', inValue, ioCA', ioVA', ioCB', ioVB') -> put $ MyState True i newPc (regs {areg = inValue} ) ioCA' ioVA' ioCB' ioVB' e
            lift $ putStrLn ("inA " ++ showHex port)
    )

inAction 0x00 regs 0x4f _ ioCB ioVB = do
    lift $ putStr "Please enter a value for port 0x00:"
    lift $ hFlush stdout
    newVal <- lift (readLn :: IO Int)
    return $ Just (regs, newVal, 0x4f, newVal, ioCB, ioVB)

inAction  _ _ _ _ _ _  = return Nothing

{-
00 is val a
01 is val b
02 is con a
03 is con b
-}

outState :: Int -> Registers -> Int -> Int -> Int -> Int -> Maybe (Registers, Int, Int, Int, Int)
--outState port regs ioControlA ioAValue ioControlB ioBValue = ???
outState 0x01 regs ioControlA ioAValue 0x0f ioBValue = Just (regs, ioControlA, ioAValue, 0x0f, areg regs)
outState 0x02 regs ioControlA ioAValue ioControlB ioBValue | areg regs == 0x4f  = Just (regs, 0x4f, ioAValue, ioControlB, ioBValue)
outState 0x03 regs ioControlA ioAValue ioControlB ioBValue | areg regs == 0x0f = Just (regs, ioControlA, ioAValue, 0x0f, ioBValue)
outState _ _ _ _ _ _ = Nothing

outAction 0x00 a = lift $ putStrLn ("NEW PORT VALUE A: " ++ showHex a)
outAction 0x01 a = lift $ putStrLn ("NEW PORT VALUE B: " ++ showHex a)
outAction 0x02 a = lift $ putStrLn ("NEW PORT CONTROL A: " ++ showHex a)
outAction 0x03 a = lift $ putStrLn ("NEW PORT CONTROL B: " ++ showHex a)
outAction _ _ = return ()


{-
LD A,h_4F;
OUT (h_02),A;
LD A,h_0F;
OUT (h_03),A;
LOOP1 : IN A,(h_00);
OUT (h_01),A;
JP LOOP1;
-}
