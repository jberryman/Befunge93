module Main
    where

import System.Process
import System.IO
import System.Exit
import Control.Monad

main = do 
      (_,testH1,_,_) <- runInteractiveCommand $
             "runhaskell Befunge.hs tests/mycology/mycology.b98"
      test1 <- hGetContents testH1
      
      (_,testH2,_,_) <- runInteractiveCommand $
             "runhaskell Befunge.hs tests/phlamethrower/compat.bf"
      test2 <- hGetContents testH2
      
      (_,testH3,_,_) <- runInteractiveCommand $
             "runhaskell Befunge.hs tests/phlamethrower/divtest.bf"
      test3 <- hGetContents testH3
      
      -- test output:
      putStr test1
      when (test1 /= goodTest1) exitFailure
      putStr test2
      when (test2 /= goodTest2) exitFailure
      putStr test3
      when (test3 /= goodTest3) exitFailure

goodTest1 = unlines $
    ["WARNING: source was truncated to 80x25; use --allow-oversize for source code of arbitrary dimensions.",
    "0 1 2 3 4 5 6 7 ",
    "GOOD: , works",
    "GOOD: : duplicates",
    "GOOD: empty stack pops zero",
    "GOOD: 2-2 = 0",
    "GOOD: | works",
    "GOOD: 0! = 1",
    "GOOD: 7! = 0",
    "GOOD: 8*0 = 0",
    "GOOD: # < jumps into <",
    "GOOD: \\ swaps",
    "GOOD: 01` = 0",
    "GOOD: 10` = 1",
    "GOOD: 900pg gets 9",
    "GOOD: p modifies space",
    "GOOD: wraparound works",
    "UNDEF: edge # skips column 80",
    "GOOD: Funge-93 spaces",
    "The Befunge-93 version of the Mycology test suite is done.",
    "Quitting..."]

goodTest2 = unlines $
    ["warning: getCell out of bounds. wrapping.",
    "Get/put wraps",
    "Cells are >8 bit",
    "Edge jumps work",
    "Negative remainders work",
    "@ in stringmode works"]

goodTest3 = ""
