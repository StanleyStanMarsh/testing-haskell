module Lib
    ( addMod
    ) where

addMod :: Int -> Int -> Int -> Int
addMod firstSummand secondSummand modulo = sum - modulo * floor(fromIntegral sum / fromIntegral modulo)
    where sum = firstSummand + secondSummand
