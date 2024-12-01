import Lib
import Test.QuickCheck

-- без Positive - исключение деление на 0 (даже на `mod`)
prop_withBuiltinAddMod :: Int -> Int -> Positive Int -> Bool
prop_withBuiltinAddMod x y (Positive m) = (addMod x y m) `mod` m == (x + y) `mod` m

prop_neutralElement :: Int -> Positive Int -> Bool
prop_neutralElement x (Positive m) = addMod x 0 m == x `mod` m

main :: IO ()
main = do
    quickCheck prop_withBuiltinAddMod

    quickCheck prop_neutralElement
    putStrLn "ALL TESTS ARE PASSED"
