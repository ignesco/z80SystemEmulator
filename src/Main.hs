module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import Data.List
import System.IO

type PCState = StateT MyState IO ()
        
data MyState = MyState { canContinue :: Bool, instructions :: [(Int, PCState)], pc  :: Int, a :: Int, ioControlA :: Int, ioAValue :: Int , ioControlB :: Int, ioBValue :: Int, e :: String }

runMachine :: [(Int, PCState)] -> IO ()
runMachine instructions  = let
            seed [] addr acc  = acc
            seed ((a, i):is) addr acc  = seed is (addr+a)  ((addr, i):acc)

            instructions' = seed instructions 0 []

            initState = MyState True instructions' 0 0 0 0 0 0 ""

        in do
            putStrLn $ show initState
            (exitedWithError, newState) <- runStateT runMe initState
            putStrLn $ show exitedWithError

runMe :: PCState
runMe = do
    MyState _ instructions pc a ioCA ioVA ioCB ioVB e <- get
    let nextInstruction' = lookup pc instructions
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
        
    let pr1 = [
                ]
    let pr2 = [

                ldA 0x4F,
                outA 0x02,

                ldA 0x0F,
                outA 0x03,
                inA 0x00,
                outA 0x01,

                halt
                ]
    runMachine pr2

instance Show MyState where
    show (MyState cont i pc a ioCA ioVA ioCB ioVB e) = concat $ intersperse " " $ [
            "canContinue =", show cont,
            "pc =", show pc,
            "a =", show a,
            "ioCA =", show ioCA,
            "ioVA =", show ioVA,
            "ioCB =", show ioCB,
            "ioVB =", show ioVB,
            "e =", show e
        ]

ldA :: Int -> (Int, PCState)
ldA num = (2, do
        MyState _ i pc a ioCA ioVA ioCB ioVB e <- get
        let newPc = (pc + 2)
        put $ MyState True i newPc num ioCA ioVA ioCB ioVB e
        lift $ putStrLn ("ldA " ++ show num)
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
            state@(MyState cont i pc a ioCA ioVA ioCB ioVB e) <- get
            let
                newPc = (pc + 2)
                res = outState port a ioCA ioVA ioCB ioVB

            case res of
                Nothing -> put $ MyState False i newPc a ioCA ioVA ioCB ioVB "ERROR"
                Just (a', ioCA', ioVA', ioCB', ioVB') -> do
                    put $ MyState True i newPc a' ioCA' ioVA' ioCB' ioVB' e
                    outAction port a'

            lift $ putStrLn ("outA " ++ show port)
    )

inA :: Int -> (Int, PCState)
inA port = (2,
        do
            state@(MyState _ i pc a ioCA ioVA ioCB ioVB e) <- get
            let
                newPc = (pc + 2)

            res <- inAction port a ioCA ioVA ioCB ioVB

            case res of
                Nothing -> put $ MyState False i pc a ioCA ioVA ioCB ioVB "ERROR"
                Just (a', ioCA', ioVA', ioCB', ioVB') -> put $ MyState True i newPc ioVA' ioCA' ioVA' ioCB' ioVB' e
            lift $ putStrLn ("inA " ++ show port)
    )

inAction 0x00 a 0x4f _ ioCB ioVB = do
    lift $ putStr "Please enter a value for port 0x00:"
    lift $ hFlush stdout
    newVal <- lift (readLn :: IO Int)
    return $ Just (a, 0x4f, newVal, ioCB, ioVB)

inAction  _ _ _ _ _ _  = return Nothing

{-
00 is val a
01 is val b
02 is con a
03 is con b
-}

outState :: Int -> Int -> Int -> Int -> Int -> Int -> Maybe (Int, Int, Int, Int, Int)
--outState port a ioControlA ioAValue ioControlB ioBValue = ???
outState 0x02 0x4f ioControlA ioAValue ioControlB ioBValue = Just (0x4f, 0x4f, ioAValue, ioControlB, ioBValue)
outState 0x03 0x0f ioControlA ioAValue ioControlB ioBValue = Just (0x0f, ioControlA, ioAValue, 0x0f, ioBValue)
outState 0x01 a ioControlA ioAValue 0x0f ioBValue = Just (a, ioControlA, ioAValue, 0x0f, a)
outState _ _ _ _ _ _ = Nothing

outAction 0x00 a = lift $ putStrLn ("NEW PORT VALUE A: " ++ show a)
outAction 0x01 a = lift $ putStrLn ("NEW PORT VALUE B: " ++ show a)
outAction 0x02 a = lift $ putStrLn ("NEW PORT CONTROL A: " ++ show a)
outAction 0x03 a = lift $ putStrLn ("NEW PORT CONTROL B: " ++ show a)
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
