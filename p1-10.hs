import           Data.List (find)

divisibleBy :: Integer -> Integer -> Bool
divisibleBy a b = a `mod` b == 0

p1 = sum [x | x <- [1..999], divisibleBy x 3 || divisibleBy x  5]

fibList :: Integer -> Integer -> [Integer]
fibList 1 1 = 1:1:2:fibList 1 2
fibList x y  = let next = x+y in next:fibList y next

fib :: [Integer]
fib = fibList 1 1

p2 = sum $ filter (\x -> x `mod` 2 == 0) $ takeWhile (<4000000) fib

isPrime :: Integer -> Bool
isPrime x = not $ any divisible $ takeWhile notTooBig [2..] where
    divisible = divisibleBy x
    notTooBig y = y*y <= x

primes :: [Integer]
primes = filter isPrime [2..]

primeFactors :: Integer -> [Integer]
primeFactors x
             | isPrime x = [x]
             | otherwise = let smallestDiv = find (divisibleBy x) primes in
                         case smallestDiv of
                           Just d -> d:primeFactors (x `div` d)
                           Nothing -> error "This should never happen!"

p3 = maximum $ primeFactors 600851475143

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

threeDigitProducts :: [Int]
threeDigitProducts = [x * y | x <- [100..999], y <- [100..999]]

p4 = maximum $ map (read::String->Integer) (filter isPalindrome $ map show threeDigitProducts)

main :: IO ()
main = do
--      let multipleOf3and5 = [x | x <- [1..999], divisibleBy x 3 || divisibleBy x  5]
--      print $ sum multipleOf3and5
     return ()
