import Lib
import Test.QuickCheck

-- без Positive - исключение деление на 0 (даже на `mod`)
prop_withBuiltinAddMod :: Int -> Int -> Positive Int -> Bool
prop_withBuiltinAddMod x y (Positive m) = (addMod x y m) `mod` m == (x + y) `mod` m

prop_neutralElement :: Int -> Positive Int -> Bool
prop_neutralElement x (Positive m) = addMod x 0 m == x `mod` m

prop_commutativity :: Int -> Int -> Positive Int -> Bool
prop_commutativity x y (Positive m) = addMod x y m == addMod y x m

prop_doubleApply :: (Eq a, Eq b, Arbitrary a, Arbitrary b) => [(a, b)] -> Bool
prop_doubleApply xs = swapTuple (swapTuple xs) == xs

prop_sameElements :: (Eq a, Arbitrary a) => [a] -> Bool
prop_sameElements xs = swapTuple (zip xs xs) == zip xs xs

prop_swapEqualityElements :: (Eq a, Eq b, Arbitrary a, Arbitrary b) => [(a, b)] -> Bool
prop_swapEqualityElements xs = all checkSwap xs
  where
    checkSwap (x, y) = 
      let swapped = swapTuple [(x, y)]
      in fst (head swapped) == y && snd (head swapped) == x

main :: IO ()
main = do
    putStrLn "FOR addMod:"
    quickCheck prop_withBuiltinAddMod

    quickCheck prop_neutralElement

    quickCheck prop_commutativity

    putStrLn "FOR swapTuple:"

    quickCheck (prop_doubleApply :: [(Int, Char)] -> Bool)

    quickCheck (prop_sameElements :: [Char] -> Bool)

    quickCheck (prop_swapEqualityElements :: [(Int, Char)] -> Bool)

    putStrLn "ALL TESTS ARE PASSED"
