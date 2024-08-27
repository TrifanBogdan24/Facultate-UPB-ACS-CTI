
-- nub = elimina duplicatele dintr-o lista
import Data.List (nub, (\\), sort)
import GHC.Base (VecElem(Int16ElemRep))

fact :: Int -> Int
fact 0 = 1
fact x = x * fact (x - 1)


mygcd :: Int -> Int -> Int
mygcd a b
    | a > b = mygcd (a - b) b
    | b > a = mygcd a (b - a)
    | otherwise = a


-- primeste o lista si returneaza o lista care nu contine duplicate
unique :: [Int] -> [Int]
unique [] = []
unique (x : xs) = nub (x : unique xs)

-- primeste o lista si returneaza o lista care nu contine duplicate
unique_v2 :: [Int] -> [Int]
unique_v2 = foldl (\cnt x -> if x `elem` cnt then cnt else cnt ++ [x]) []


contains :: [Int] -> Int -> Bool
contains [] _ = False
contains (x : xs) val = if x == val then True else contains xs val


getUniques :: [Int] -> [Int]
getUniques xs = filter (\x -> count xs x == 1) (nub xs)
    where
        count :: [Int] -> Int -> Int
        count [] _ = 0
        count (y : ys) n = (if y == n then 1 else 0) + count ys n



mySqrt :: Int -> Int
mySqrt = floor . sqrt. fromIntegral



radical :: Int -> Int
radical n = cautare 0 n
    where
        cautare :: Int -> Int -> Int
        cautare left right
            | left > right = right
            | mid * mid <= n = cautare (mid + 1) right
            | otherwise = cautare left (mid - 1)
            where
                mid = (left + right) `div` 2



myMin :: [Int] -> Int
myMin [] = error "Empty list"
myMin [x] = x
myMin (x : xs) = if x < val then x else val     -- min x val
    where val = myMin xs

myMax :: [Int] -> Int
myMax [] = error "Empty list"
myMax [x] = x
myMax (x : xs) = if x > val then x else val     -- max x val
    where val = myMax xs


fizzBuzz :: [Int] -> [String]
fizzBuzz [] = []
fizzBuzz (x : xs)
    | x `mod` 15 == 0 = "FizzBuzz" : fizzBuzz xs
    | x `mod` 5 == 0 = "Buzz" : fizzBuzz xs
    | x `mod` 3 == 0 = "Fizz" : fizzBuzz xs
    | otherwise = show x : fizzBuzz xs


fizzBuzzBazz :: [Int] -> [String]
fizzBuzzBazz [] = []
fizzBuzzBazz (x : xs)
    | x `mod` 105 == 0 = "FizzBuzzBazz" : fizzBuzzBazz xs
    | x `mod` 35 == 0 = "BuzzBazz" : fizzBuzzBazz xs
    | x `mod` 21 == 0 = "FizzBazz" : fizzBuzzBazz xs
    | x `mod` 15 == 0 = "FizzBuzz" : fizzBuzzBazz xs
    | x `mod` 7 == 0 = "Bazz" : fizzBuzzBazz xs
    | x `mod` 5 == 0 = "Buzz" : fizzBuzzBazz xs
    | x `mod` 3 == 0 = "Fizz" : fizzBuzzBazz xs
    | otherwise = show x : fizzBuzzBazz xs

-- s1 intoarce True numai pt 1 si 2
s1 1 = True
s1 2 = True
s1 _ = False
 
s2 x = mod x 2 == 0
 
s3 _ = False


mem :: (Integer -> Bool) -> Integer -> Bool
mem set val = set val


isPow2 :: Integer -> Bool
isPow2 0 = False
isPow2 1 = True
isPow2 x = if x `mod` 2 == 1 then False else isPow2 (x `div` 2)


setOfPows2 x = isPow2 x



isNat :: Int -> Bool
isNat x
    | x >= 0 = True
    | otherwise = False

setOfNats x = isNat x


intersection :: (Integer -> Bool) -> (Integer -> Bool) -> (Integer -> Bool)
intersection s1 s2 = \x -> s1 x && s2 x

intersection' :: (Integer -> Bool) -> (Integer -> Bool) -> Integer -> Bool
intersection' s1 s2 x = (s1 x) && (s2 x)


toSet :: [Integer] -> (Integer -> Bool)
toSet list = \x -> x `elem` list 

{-
    a = tipul elementelor din lista
    b = tipul acumulatorului si al listei rezultat

    * 1st arg = functia de reducere
        * 1st arg func = acc
        * 2nd arg func = val
    * 2nd arg = valoarea initiala a acumulatorului
    * 3nd arg = list
    foldr :: (a -> b -> b) -> b -> [a] -> b
-}
capList :: [Integer -> Bool] -> Integer -> Bool
capList [] = \x -> False   -- const False
capList (x : xs) = foldr intersection' x xs


capList' :: [Integer -> Bool] -> Integer -> Bool
capList' [] = \x -> False

-- elementul curent al listei va fi o functie Integer -> Bool
capList' (x : xs) = \val -> x val || inter val
    where
        inter = capList' xs


setsOperation :: [Integer -> Bool] -> ((Integer -> Bool) -> (Integer -> Bool) -> (Integer -> Bool)) -> (Integer -> Bool)
setsOperation [] _ = \x -> False
                            -- foldr functia_de_reducere acc_init_val list
setsOperation (x : xs) func = foldr func x xs


applySet :: (Integer -> Bool) -> [Integer] -> [Integer]
applySet _ [] = []
applySet func list = filter func list


partitionSet :: (Integer -> Bool) -> [Integer] -> ([Integer], [Integer])
partitionSet _ [] = ([] ,[])
partitionSet func set = (applySet func set, applySet (\x -> if func x == True then False else True) set)


mymapr :: (a -> b) -> [a] -> [b]
mymapr func = foldr (\x acc -> [func x] ++ acc) []

mymapl :: (a -> b) -> [a] -> [b]
mymapl func = foldl (\acc x -> acc ++ [func x]) []


myfilterl :: (a -> Bool) -> [a] -> [a]
myfilterl func list = foldl (\acc val -> if func val == True then acc ++ [val] else acc) [] list


myfilterr :: (a -> Bool) -> [a] -> [a]
myfilterr func list = foldr (\val acc -> if func val == True then [val] ++ acc else acc) [] list


bubbleSort :: [Int] -> [Int]
bubbleSort list =
    if isSorted parcurgere == True then parcurgere else bubbleSort parcurgere
    
    where
        helper :: [Int] -> [Int]
        helper [] = []
        helper [x] = [x]
        helper (x1 : (x2 : xs)) =
            if x1 > x2 then x2 : helper (x1 : xs)
            else x1 : helper (x2 : xs)

        parcurgere :: [Int]
        parcurgere = helper list


        isSorted :: [Int] -> Bool
        isSorted [x] = True
        isSorted (x1 : x2 : xs) = if x1 > x2 then False else isSorted (x2 : xs)



-- quickSort :: [Int] -> [Int]



main :: IO ()
main = do 
    putStrLn $ "fact(1) = " ++ show (fact 1)
    putStrLn $ "fact(5) = " ++ show (fact 5)
    putStrLn $ "fact(10) = " ++ show (fact 10)


    putStrLn $ "gcd(393, 909) = " ++ show (gcd 393 909) 
    putStrLn $ "gcd(500, 1000) = " ++ show (gcd 500 1000) 
    putStrLn $ "gcd(1000, 500) = " ++ show (gcd 1000 500) 
    putStrLn $ "gcd(21, 17) = " ++ show (gcd 21 17)


    -- afisarea unei liste de intregi
    putStrLn $ show (1 : 2 : 3 : 4 : 2 : 5: 6 : [])
    
    putStrLn $ show $ unique_v2 (1 : 2 : 3 : 4 : 2 : 5: 6 : [])
    print $  unique (1 : 2 : 3 : 4 : 2 : 5: 6 : [])

    putStrLn $ show $ getUniques (1 : 2 : 3 : 4 : 2 : 5: 6 : [])
    putStrLn $ show $ myMin (7 : 2 : 8  : 0: 4 : 2 : 5: 6 : [])
    print $ myMax (7 : 2 : 8  : 0 : 101 : 4 : 2 : 5: 6 : [])


    print $ fizzBuzz [1..20]

    print $ fizzBuzzBazz[1..120]

    print $ bubbleSort (1 : 20 : 3 : 40 : 5: 1: [])
