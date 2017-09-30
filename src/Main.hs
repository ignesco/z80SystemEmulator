module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer        

runMachine :: MyState -> [(Int, PCState)] -> IO ()
runMachine initState instructions  = let
            seed [] addr acc  = acc
            seed ((a, i):is) addr acc  = seed is (addr+a)  ((addr, i):acc)

            instructions' = seed instructions 0 []
        in do
            putStrLn $ show initState
            runMachine' initState instructions'

        
runMachine' :: MyState -> [(Int, PCState)] -> IO ()
runMachine' initState instructions = let
            pc' = pc initState
        
        in do
            let instr  = lookup pc' instructions
            (canContinue, newState) <- maybe (return (False, initState)) (\i -> runStateT i initState) (lookup pc' instructions)
            putStrLn $ show newState
            if canContinue then runMachine' newState instructions else return ()

dodebug :: String -> PCState
dodebug s = do
        lift $ putStrLn s
        return True


debug :: String -> (Int, PCState) -> (Int, PCState)
debug s (x, instruction) = (x, dodebug s >> instruction)
        
--main :: IO ()
main = do

{-
LD A,h_4F;
OUT (h_02),A;
LD A,h_0F;
OUT (h_03),A;
LOOP1 : IN A,(h_00);
OUT (h_01),A;
JP LOOP1;
-}
        
    let pr = [
                ldA 50,
                ldA 99,
                outA 50,
                halt
                ]
        initState = MyState 0 0 0 0 0 0 ""
        
    runMachine initState pr
--    return ()


prog = do
    ldA 50
--    ldA 150

type PCState = StateT MyState IO Bool
        
data MyState = MyState { pc  :: Int, a :: Int, ioControlA :: Int, ioAValue :: Int , ioControlB :: Int, ioBValue :: Int, e :: String } deriving Show

ldA :: Int -> (Int, PCState)
ldA num = (2, do
        MyState pc a ioCA ioVA ioCB ioVB e <- get
        let newPc = (pc + 2)
        put $ MyState newPc num ioCA ioVA ioCB ioVB e
        lift $ putStrLn ("ldA " ++ show num)
        return True )

halt :: (Int, PCState)
halt = (1, do
        MyState pc a ioCA ioVA ioCB ioVB e <- get
        let newPc = (pc + 1)
        put $ MyState newPc a ioCA ioVA ioCB ioVB "HALT"
        lift $ putStrLn "halt"
        return False)

outA :: Int -> (Int, PCState)
outA num = (2, do
        MyState pc a ioCA ioVA ioCB ioVB e <- get
        let newPc = (pc + 2)
        if num == 50
          then do
            put $ MyState newPc a num ioVA ioCB ioVB e
            return True
          else do
            put $ MyState newPc a ioCA ioVA ioCB ioVB "ERROR"
            return False)
        