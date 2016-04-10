
divisibleBy :: Integer -> Integer -> Bool
divisibleBy a b = mod a b == 0


main :: IO ()
main = do
     let multipleOf3and5 = [x | x <- [1..999], divisibleBy x 3 || divisibleBy x  5]
     print multipleOf3and5
     print $ sum multipleOf3and5
     return ()
