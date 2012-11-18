-- ICL.hs (Interactive CL)

module ICL where

import System.IO
import Control.Monad
import System.Console.Editline.Readline

import CL
import Expr
import StdLib (stdlib)
import ParseCL
import CLError

flushStr :: String -> IO ()
flushStr str = putStr str  >> hFlush stdout

evalString :: String -> IO String
evalString s = return $ extractValue $ trapError (liftM show $ eval stdlib s)

evalAndPrint :: String -> IO ()
evalAndPrint s = evalString s >>= putStrLn

repl :: IO ()
repl = do
   maybeLine <- readline "CL>> "
   case maybeLine of 
    Nothing     -> return () -- EOF / control-d
    Just "quit" -> return ()
    Just line -> do addHistory line
                    evalAndPrint line
                    repl





