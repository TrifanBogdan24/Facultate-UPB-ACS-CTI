-- Structura de date pentru arborele binar
-- `derivingShow` -> pt afisarea structurii
data BTree = Node Int BTree BTree | Nil deriving Show

data StreamBTree = StreamNode Int StreamBTree StreamBTree

nats :: [Integer]
nats = [0..]            -- lista infinita incepand de la 0

odds :: [Integer]
odds = [1, 3 ..]        -- lista infinita cu nr IMPARE

squares :: [Integer]
squares = [x * x | x <- nats]   -- nats = [0..]


-- Funcția care generează o listă infinită de numere prime
primes :: [Integer]
primes = sieve [2..]
  where
    sieve :: [Integer] -> [Integer]
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]


fibs :: [Double]  -- type casting la Double
{-
sintaxa == 1stEL : 2ndEL : restulListei
-}
fibs = 0.0 : 1.0 : zipWith (+) fibs (tail fibs)  -- numere zecimale

-- sliceTree :: Int -> StreamBTree -> BTree
-- Parametri: k (Int), t (StreamBTree)
-- Retur: BTree
sliceTree 0 _ = Nil
sliceTree k (StreamNode x left right) = Node x (sliceTree (k-1) left) (sliceTree (k-1) right)

-- repeatTree :: Int -> StreamBTree
-- Parametru: k (Int)
-- Retur: StreamBTree
repeatTree k = StreamNode k (repeatTree k) (repeatTree k)

-- generateTree :: Int -> (Int -> Int) -> (Int -> Int) -> StreamBTree
-- Parametri: k (Int), leftF (Int -> Int), rightF (Int -> Int)
-- Retur: StreamBTree
generateTree k leftF rightF = StreamNode k (generateTree (leftF k) leftF rightF) (generateTree (rightF k) leftF rightF)

-- build :: (Double -> Double) -> Double -> [Double]
-- Parametri: f (Double -> Double), x (Double)
-- Retur: [Double]
build f x = x : build f (f x)

-- alternatingBinary :: [Double]
alternatingBinary = build (\x -> if even (floor x) then 1.0 else 0.0) 0.0
-- Expresie lambda: \x -> if even (floor x) then 1.0 else 0.0

-- alternatingCons :: [Double]
alternatingCons = build (\x -> if even (floor x) then x else -x) 0.0
-- Expresie lambda: \x -> if even (floor x) then x else -x

-- alternatingPowers :: [Double]
alternatingPowers = build (\x -> if even (floor x) then (-2) ** x else 2 ** x) 1.0
-- Expresie lambda: \x -> if even (floor x) then (-2) ** x else 2 ** x
-- Expresie lambda: \param1 param2 -> body

-- select :: Double -> [Double] -> Double
-- Parametri: e (Double), xs ([Double])
-- Retur: Double
select e (x : y : xs)
    | abs (x - y) < e = y
    | otherwise = select e (y : xs)

-- phiApprox :: Double
phiApprox = select 0.00001 (zipWith (/) (tail fibs) fibs)

-- piApprox :: Double
piApprox = select 0.00001 (build (\x -> x + sin x) 3.0)
-- Expresie lambda: \x -> x + sin x

-- sqrtApprox :: Double -> Double
-- Parametru: k (Double)
-- Retur: Double
sqrtApprox k = select 0.00001 (build (\x -> 0.5 * (x + k / x)) (k / 2))
-- Expresie lambda: \x -> 0.5 * (x + k / x)

-- derivativeApprox :: (Double -> Double) -> Double -> Double
-- Parametri: f (Double -> Double), a (Double)
-- Retur: Double
derivativeApprox f a = select 0.00001 (map (\h -> (f (a + h) - f a) / h) (build (/2) 1.0))
-- Expresie lambda: \h -> (f (a + h) - f a) / h

main :: IO ()

{-
reminder: take n list   -> ruturneaza primele n numere din lista
-}
main = do
    putStrLn "Primele 10 numere Fibbonaci:"
    print (take 10 fibs)
    putStrLn "\n"

    putStrLn "Patratele numerelor 1-10:"
    print (take 10 squares)
    putStrLn "\n"

    putStrLn "Slice of infinite tree (level 2):"
    print (sliceTree 2 (repeatTree 1))
    putStrLn "\n"

    putStrLn "aprox. Golden Ratio (phi):"
    print phiApprox
    putStrLn "\n"


    putStrLn "aprox. pi:"
    print piApprox
    putStrLn "\n"

    putStrLn "aprox. sqrt(2) :"
    print (sqrtApprox 2.0)
    putStrLn "\n"

    putStrLn "f(x) = (sin(x))'"
    putStrLn "aprox. of  f(0):"
    print (derivativeApprox sin 0.0)
    putStrLn "\n"
