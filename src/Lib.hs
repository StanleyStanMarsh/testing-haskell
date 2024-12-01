module Lib
    ( addMod
    , swapTuple
    ) where

addMod :: Int -> Int -> Int -> Int
addMod firstSummand secondSummand modulo = sum - modulo * floor(fromIntegral sum / fromIntegral modulo)
    where sum = firstSummand + secondSummand

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

swapTuple :: [(a, b)] -> [(b, a)]
swapTuple tupleList = map swap tupleList