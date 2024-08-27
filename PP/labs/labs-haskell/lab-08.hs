-- GHCI = Glasglow Haskell Compilation Interactive

-- acesta este un comentariu pe un rand



{- multi line comment -}

{-
comentariu pe
mai multe randuri
1
23
4
-}




{-
cum folosim interpretorul in terminal
`$` -> inceputul unei linii in BASH SHELL
`>` -> inceputul unei linii in GHCI


$ ghc lab-08.hs     # compilare
$ ./lab-08.hs       # executare (rulare executabil)
$ ghci    # pornire interpretor

>  -- comentariu in interpretor
> :l file.hs      # importam functiile/variabilele dintr-un fisier in interpretor
> :t function     # afiseaza signatura functiei


-}


-- continut laborator
functie1 :: [Int] -> Bool
functie1 param    -- folosim `|` pentru a face pattern matching
  | param == [] = False
  | mod (head param) 2 == 1 = False
  | otherwise = True


functie2 :: [Int] -> Bool
functie2 param = case param of
  [] -> False
  (x : []) -> False
  (x1 : x2 : l2) -> True


func3 :: [Int] -> Bool
func3 [] = False
func3 (x : []) = False
func3 (x1 : x2 : el) = True



fact_tail:: Int -> Int
fact_tail n = aux n 1
  where 
    aux :: Int -> Int-> Int
    aux 0 acc = acc
    aux n acc = aux (n - 1) (n * acc)






-- 8.1.1
fact :: Int -> Int
fact n = go n 1
  where
    go 0 acc = acc
    go m acc = go (m - 1) (m * acc)


-- 8.1.1
-- pattern matching 
fact2 :: Int -> Int
fact2 0 = 1
fact2 n = n * fact2 (n - 1)


-- 8.1.2. GCD (Greatest Common Divisor) = CMDDC (Cel mai mare divizor comun) 
mygcd :: Int -> Int -> Int
mygcd a b
  | b == 0 = a
  | otherwise = mygcd b (a `mod` b)

-- 8.1.3. (sqrt)
mySqrt :: Int -> Int
mySqrt = floor . sqrt . fromIntegral

-- 8.1.4. (minimul unei liste)
mymin :: [Int] -> Int
mymin [] = error "Empty list"
mymin [x] = x
mymin (x:xs) = min x (mymin xs)

-- 8.1.4. (valoarea maxima a unei liste)
mymax :: [Int] -> Int
mymax [] = error "Empty list"
mymax [x] = x
mymax (x:xs) = max x (mymax xs)

-- 8.1.5 (lista rezultata va contine doar elementele unice)
unique :: [Int] -> [Int]
unique = foldl (\count x -> if x `elem` count then count else count ++ [x]) []

-- 8.1.6
fizzBuzz :: Int -> String
fizzBuzz n
    | n `mod` 105 == 0 = "FizzBuzzBazz"
    | n `mod` 35 == 0 = "BuzzBazz"
    | n `mod` 21 == 0 = "FizzBazz"
    | n `mod` 7 == 0 = "Bazz"
    | n `mod` 5 == 0 && n `mod` 3 == 0 = "FizzBuzz"
    | n `mod` 3 == 0 = "Fizz"
    | n `mod` 5 == 0 = "Buzz"
    | otherwise = show n

-- 8.1.7.
fizzBuzzExtended :: Int -> String
fizzBuzzExtended n
  | n `mod` 105 == 0 = "FizzBuzzBazz"
  | n `mod` 35 == 0 = "BuzzBazz"
  | n `mod` 21 == 0 = "FizzBazz"
  | n `mod` 7 == 0 = "Bazz"
  | n `mod` 5 == 0 && n `mod` 3 == 0 = "FizzBuzz"
  | n `mod` 3 == 0 = "Fizz"
  | n `mod` 5 == 0 = "Buzz"
  | otherwise = show n


-- 8.1.8.
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x:xs) = foldl' f (f z x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

mem :: (Integer -> Bool) -> Integer -> Bool
mem s x = s x

-- 8.2.2.
powersOfTwo :: Integer -> Bool
powersOfTwo n = n `mod` 2 == 0

-- 8.2.3.
naturalNumbers :: Integer -> Bool
naturalNumbers _ = True

-- 8.2.4.
intersection :: (Integer -> Bool) -> (Integer -> Bool) -> (Integer -> Bool)
intersection s1 s2 = \x -> s1 x && s2 x

-- 8.2.5
intersection' :: (Integer -> Bool) -> (Integer -> Bool) -> Integer -> Bool
intersection' s1 s2 x = s1 x && s2 x

-- 8.2.6.
toSet :: [Integer] -> (Integer -> Bool)
toSet xs = \x -> x `elem` xs


-- 8.2.7
capList :: [Integer -> Bool] -> Integer -> Bool
capList sets = foldr1 intersection sets


-- 8.2.8. 
capList' :: [Integer -> Bool] -> Integer -> Bool
capList' [] _ = True
capList' (s:ss) x = s x && capList' ss x

-- 8.2.9
setsOperation :: [Integer -> Bool] -> ((Integer -> Bool) -> (Integer -> Bool) -> (Integer -> Bool)) -> (Integer -> Bool)
setsOperation sets op = foldr1 op sets


-- 8.2.10
applySet :: (Integer -> Bool) -> [Integer] -> [Integer]
applySet s = filter s


-- 8.2.11
partitionSet :: (Integer -> Bool) -> [Integer] -> ([Integer], [Integer])
partitionSet s xs = (filter s xs, filter (not . s) xs)


-- 8.3.1
mymapl :: (a -> b) -> [a] -> [b]
mymapl f = foldr (\x acc -> f x : acc) []


-- 8.3.2
mymapr :: (a -> b) -> [a] -> [b]
mymapr f = foldl (\acc x -> acc ++ [f x]) []


-- 8.3.3
myfilterl :: (a -> Bool) -> [a] -> [a]
myfilterl p = foldl (\acc x -> if p x then acc ++ [x] else acc) []

myfilterr :: (a -> Bool) -> [a] -> [a]
myfilterr p = foldr (\x acc -> if p x then x : acc else acc) []

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f z xs = foldr (\x acc -> \z' -> acc (f z' x)) id xs z


-- 8.3.4.
bubbleSort :: [Int] -> [Int]
bubbleSort = foldr bubble []
    where bubble x [] = [x]
          bubble x (y:ys)
            | x <= y    = x:y:ys
            | otherwise = y:bubble x ys

-- 8.3.5. 
quickSort :: [Int] -> [Int]
quickSort []     = []
quickSort (x:xs) = quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]


main :: IO ()
main = do
  putStrLn "Hello, World!"
  
  -- 8.1.1
  putStr "5! (factorial) = "
  putStrLn $ "  " ++ show (fact 5)

  -- 8.1.1
  -- please make this wokr
  putStrLn $ "5! (factorial) = " ++ show (fact 5)


  -- 8.1.2
  putStr "GCD of 24 and 36 = "
  putStrLn $ "  " ++ show (mygcd 24 36)
  putStrLn $ "CMDDC(101, 17) = " ++ show (mygcd 101 17)

  -- 8.1.3.
  putStr "sqrt(25) = :"
  putStrLn $ "  " ++ show (mySqrt 25)
  putStrLn $ "sqrt(34) = " ++ show (mySqrt 34)
  putStrLn $ "sqrt(36) = " ++ show (mySqrt 36)


  -- 8.1.4 (min)
  putStr "min( [5, 3, 9, 1] )= "
  putStrLn $ "  " ++ show (mymin [5, 3, 9, 1])
  putStrLn $ "min( [1, 2, 3] ) = " ++ show (mymin (1:2:3:[]))

  -- 8.1.4 (max)
  putStr "max of [5, 3, 9, 1]: = "
  putStrLn $ "  " ++ show (mymax [5, 3, 9, 1])
  putStrLn $ "max( [1, 2, 3] ) = " ++ show (mymax (1:2:3:[]))

  -- 8.1.5
  putStr "unice [1, 2, 2, 3, 3, 3] = "
  putStrLn $ "  " ++ show (unique [1, 2, 2, 3, 3, 3])
  putStrLn $ "unice [1, 1, 1] = " ++ show (unique (1:1:1:[]))

  -- 8.1.6
  putStrLn "FizzBuzz 1-15:"
  mapM_ (putStrLn . fizzBuzz) [1..15]
  mapM_ putStrLn (map fizzBuzz [1..15])


  -- 8.1.7.
  putStrLn "FizzBuzzExtended 1-15:"
  mapM_ (putStrLn . fizzBuzzExtended) [1..15]
  mapM_ putStrLn (map fizzBuzzExtended [1..15])


-- 8.1.8.
  putStrLn "Foldl'  = "
  putStrLn $ "  " ++ show (foldl' (+) 0 [1..10])

  putStrLn "Foldr'  = "
  putStrLn $ "  " ++ show (foldr' (+) 0 [1..10])

  putStrLn "Filter'  = "
  putStrLn $ "  " ++ show (filter' even [1..10])

  putStrLn "Map'  = "
  putStrLn $ "  " ++ show (map' (*2) [1..5])

  -- 8.2.2.
  putStrLn "Powers of two:"
  putStrLn $ "  " ++ show (powersOfTwo 4)
  
  -- 8.2.3.
  putStrLn "Natural numbers:"
  putStrLn $ "  " ++ show (naturalNumbers 5)

  -- 8.2.4.
  putStrLn "Intersection  = "
  putStrLn $ "  " ++ show (intersection (\x -> x `mod` 2 == 0) (\x -> x `mod` 3 == 0) 6)

  putStrLn "Intersection'  = "
  putStrLn $ "  " ++ show (intersection' (\x -> x `mod` 2 == 0) (\x -> x `mod` 3 == 0) 6)

  -- 8.2.6.
  putStrLn "ToSet  = "
  let mySet = toSet [1,2,3,4]
  putStrLn $ "  1 is in set: " ++ show (mySet 1)
  putStrLn $ "  5 is in set: " ++ show (mySet 5)

  putStrLn "CapList  = "
  let sets = [(\x -> x `mod` 2 == 0), (\x -> x `mod` 3 == 0)]
  putStrLn $ "  Intersection of sets: " ++ show (capList sets 6)

  putStr "CapList'  = "
  let sets' = [(\x -> x `mod` 2 == 0), (\x -> x `mod` 3 == 0)]
  putStrLn $ "  Intersection of sets: " ++ show (capList' sets' 6)

  putStr "SetsOperation = "
  let setsOp = [(\x -> x `mod` 2 == 0), (\x -> x `mod` 3 == 0), (\x -> x `mod` 5 == 0)]

  putStr "ApplySet  = "
  putStrLn $ "  Applying set to list: " ++ show (applySet (\x -> x `mod` 2 == 0) [1..10])

  putStr "PartitionSet  = "
  let (evens, odds) = partitionSet (\x -> x `mod` 2 == 0) [1..10]
  putStrLn $ "  Evens: " ++ show evens
  putStrLn $ "  Odds: " ++ show odds

  -- 8.3.1
  putStrLn $ "MyMapL  = " ++ show (mymapl (+1) [1..5])

  -- 8.3.2
  putStrLn $ "MyMapR  = " ++ show (mymapr (+1) [1..5])


  -- 8.3.3
  putStrLn $ "MyFilterL  = " ++ show (myfilterl even [1..10])

  putStrLn $ "MyFilterR  = " ++ show (myfilterr even [1..10])

  putStrLn $ "MyFoldL  = " ++ show (myfoldl (+) 0 [1..10])

  putStrLn $ "BubbleSort  = " ++ show (bubbleSort [5,2,10,4,1])

  putStrLn $ "QuickSort  = " ++ show (quickSort [5,2,10,4,1])
