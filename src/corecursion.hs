main :: IO ()
main =
   print (fibonacciRecursive <$> [1..10])
   >> print (fibonacciCorecursive <$> [1..10])

factorialRecursive :: Int -> Int
factorialRecursive 0 = 1
factorialRecursive n = n * (factorialRecursive (n-1))

factorialCorecursive :: Int -> Int
factorialCorecursive i =
  let
    factorial :: [(Int, Int)]
    factorial =
      (\(n,f) -> (n+1, f*(n+1))) `iterate` (0,1)
  in
     snd $ factorial !! i

fibonacciRecursive :: Int -> Int
fibonacciRecursive n | n <= 1 = 1
                     | otherwise = (fibonacciRecursive (n-2)) + (fibonacciRecursive (n-1))

fibonacciCorecursive :: Int -> Int
fibonacciCorecursive i =
  let
    helper = (\(f1,f2) -> (f2,f1+f2)) `iterate` (0,1)
  in
    snd $ helper !! i
