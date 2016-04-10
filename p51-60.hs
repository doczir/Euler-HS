import Data.Char (digitToInt)
import Data.List (find)
import Data.Maybe (fromMaybe)

numDigits :: Int -> Int
numDigits = length . show

divisibleBy :: Int -> Int -> Bool
divisibleBy a b = a `mod` b == 0

subsets :: [Int] -> [[Int]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

whitemask :: Int -> [Int] -> [Int]
whitemask num places = map replace digits where
          digits = if 1 `elem` places then [1..9] else [0..9]
          toList = map digitToInt $ show num
          replace digit = foldl toNum 0 $ zip toList [1..] where
                  toNum r (d,i) = 10*r + fromIntegral ( if i `elem` places then digit else d )

isPrime :: Int -> Bool
isPrime x = not $ any divisible $ takeWhile notTooBig [2..] where
    divisible = divisibleBy x
    notTooBig y = y*y <= x

primes :: [Int]
primes = filter isPrime [2..]

primeValueFamily :: Int -> [[Int]]
primeValueFamily x = map (filter isPrime . whitemask x) digits where
                 digits = subsets [1..(numDigits x)]

p51 :: Int
p51 = fromMaybe (error "This is just dumb...") (find (\x -> any (\y -> length y == 8 && x `elem` y) (primeValueFamily x)) ( drop 10000 primes ))

main :: IO ()
main = do
     print p51
     return ()
