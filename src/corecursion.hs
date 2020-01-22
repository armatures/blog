main :: IO ()
main =
   print (factorialCorecursive 5)
   >> print (factorialRecursive 5)

factorialCorecursive :: Int -> Int
factorialCorecursive i =
  let
    factorial :: [(Int, Int)]
    factorial =
      (\(n,f) -> (n+1, f*(n+1))) `iterate` (0,1)
  in
     snd $ factorial !! i


factorialRecursive :: Int -> Int
factorialRecursive 0 = 1
factorialRecursive n = n * (factorialRecursive (n-1))
