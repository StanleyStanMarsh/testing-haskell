import Lib
import Test.QuickCheck

-- без Positive - исключение деление на 0 (даже на `mod`)
prop_withBuiltinAddMod x y (Positive m) = (addMod x y m) `mod` m == (x + y) `mod` m

main :: IO ()
main = do
    quickCheck prop_withBuiltinAddMod
    putStrLn "OK"
