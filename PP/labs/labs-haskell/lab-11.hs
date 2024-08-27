import Data.List (sort)
import Control.Monad (guard)
import Control.Applicative (Alternative(..))

-- 11.1. Working with Maybe

-- 11.1.1. add5 using fmap
add5 :: Maybe Int -> Maybe Int
add5 = fmap (+5)

-- 11.1.2. add, sub, and mult using do notation
add :: Maybe Int -> Maybe Int -> Maybe Int
add ma mb = do
    a <- ma
    b <- mb
    return (a + b)

sub :: Maybe Int -> Maybe Int -> Maybe Int
sub ma mb = do
    a <- ma
    b <- mb
    return (a - b)

mult :: Maybe Int -> Maybe Int -> Maybe Int
mult ma mb = do
    a <- ma
    b <- mb
    return (a * b)

-- 11.2. Working with IO

-- 11.2.1. Hello World program
helloWorld :: IO ()
helloWorld = putStrLn "Hello, World!"

-- 11.2.2. Fibonacci number
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

fibonacciIO :: IO ()
fibonacciIO = do
    putStrLn "Scrie mai jos un numar intreg:"
    input <- getLine
    let n = read input :: Int
    print (fibonacci n)

-- 11.2.3. Read and sort a list of numbers
sortNumbersIO :: IO ()
sortNumbersIO = do
    putStrLn "Scire (pe randul de dedesubt) numarul de elemente al array-ului:"
    inputN <- getLine
    let n = read inputN :: Int
    putStrLn $ "Introdu " ++ show n ++ " numbere (cate unul pe o linie):"
    numbers <- sequence $ replicate n (readLn :: IO Int)
    let sortedNumbers = sort numbers
    putStrLn "Numerele ordonate:"
    print sortedNumbers

-- 11.2.4. Collatz sequence
collatz :: Int -> [Int]
collatz 1 = [1]
collatz n
    | even n = n : collatz (n `div` 2)
    | odd n  = n : collatz (3 * n + 1)

collatzIO :: IO ()
collatzIO = do
    putStrLn "Introdu mai jos un numar:"
    input <- getLine
    let n = read input :: Int
    putStrLn "secv. Collatz:"
    print (collatz n)

-- 11.3. Lists

type Pos = (Int, Int)

-- 11.3.1. Knight's moves
moveKnight :: Pos -> [Pos]
moveKnight (x, y) = do
    (dx, dy) <- [(2, 1), (2, -1), (-2, 1), (-2, -1), (1, 2), (1, -2), (-1, 2), (-1, -2)]
    let (x', y') = (x + dx, y + dy)
    guard (x' `elem` [1..8] && y' `elem` [1..8])
    return (x', y')

-- 11.3.2. Reach target position in 3 moves
canReachIn3 :: Pos -> Pos -> Bool
canReachIn3 start end = end `elem` in3 start
    where
        in3 pos = return pos >>= moveKnight >>= moveKnight >>= moveKnight

-- 11.3.3. Reach target position in k moves
canReachInK :: Int -> Pos -> Pos -> Bool
canReachInK 0 start end = start == end
canReachInK k start end = end `elem` inK k start
    where
        inK 0 pos = return pos
        inK k pos = inK (k-1) pos >>= moveKnight

-- 11.4. Probability Distributions

newtype Prob a = Prob [(a, Float)] deriving Show

-- 11.4.1. Functor instance for Prob
instance Functor Prob where
    fmap f (Prob xs) = Prob [(f a, p) | (a, p) <- xs]

prob :: Prob Int
prob = Prob [(1, 0.5), (2, 0.25), (3, 0.25)]

-- 11.4.2. Flatten function
flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob [(a, p * q) | (Prob inner, p) <- xs, (a, q) <- inner]

nestedProb :: Prob (Prob Int)
nestedProb = Prob [(prob, 0.7), (prob, 0.3)]


instance Monad Prob where
  -- remove the definition for `return`
    m >>= f = flatten (fmap f m)

-- Applicative instance for Prob
instance Applicative Prob where
    pure x = Prob [(x, 1.0)]  -- move the definition here
    Prob fs <*> Prob xs = Prob [(f x, p * q) | (f, p) <- fs, (x, q) <- xs]

-- Alternative instance for Prob
instance Alternative Prob where
    empty = Prob []
    Prob xs <|> Prob ys = Prob (xs ++ ys)

-- 11.4.4. Fair n-sided die
die :: Int -> Prob Int
die n = Prob [(i, 1 / fromIntegral n) | i <- [1..n]]

-- 11.4.5. M3 problem
data TestResult = Positive | Negative deriving (Show, Eq)

diseaseProb :: Prob Bool
diseaseProb = Prob [(True, 0.01), (False, 0.99)]

test :: Bool -> Prob TestResult
test True  = Prob [(Positive, 0.95), (Negative, 0.05)]
test False = Prob [(Positive, 0.05), (Negative, 0.95)]

posterior :: Prob Bool
posterior = do
    hasDisease <- diseaseProb
    result <- test hasDisease
    guard (result == Positive)
    return hasDisease

probabilityDiseaseGivenPositive :: Float
probabilityDiseaseGivenPositive = sum [p | (True, p) <- xs]
    where Prob xs = posterior

-- Main function to demonstrate all functionalities
main :: IO ()
main = do
    -- 11.1. Maybe examples
    print $ add5 (Just 10)       -- Should print Just 15
    print $ add (Just 10) (Just 5)  -- Should print Just 15
    print $ sub (Just 10) (Just 5)  -- Should print Just 5
    print $ mult (Just 10) (Just 5) -- Should print Just 50

    -- 11.2. IO examples
    helloWorld                    -- Should print "Hello, World!"
    fibonacciIO                   -- Should read an integer and print the Fibonacci number
    sortNumbersIO                 -- Should read a list of integers and print them sorted
    collatzIO                     -- Should read an integer and print the Collatz sequence

    -- 11.3. List examples
    print $ moveKnight (4, 4)     -- Should print possible knight moves from position (4,4)
    print $ canReachIn3 (1, 1) (2, 3) -- Should print True or False
    print $ canReachInK 3 (1, 1) (8, 8) -- Should print True or False

    -- 11.4. Probability examples
    print $ fmap (+3) prob        -- Should print Prob [(4, 0.5), (5, 0.25), (6, 0.25)]
    print $ flatten nestedProb   -- Should print Prob [(1,0.35),(2,0.175),(3,0.175),(1,0.15),(2,0.075),(3,0.075)]
    print probabilityDiseaseGivenPositive -- Should print the probability Jo has the disease given a positive test result
