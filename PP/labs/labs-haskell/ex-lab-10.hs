-- `type` alias (nume asociate unor tipuri de date deja existente)
type Matrix = [[Int]]
type Name = String
type PhoneNumber = String
type PhoneBook = [(PhoneNumber, Name)]

    -- map

{-
`data` = un tip nou de date
Constructorii sunt sperati in definita tipului prin operatorul pipe `|`
Fiecare constructor poate primi parametri
Tipul de date poate fi conceput drept un tip generic de date `[A]`
-}


-- constructorii tipului BinaryDigit: Zero si One
-- `enum` (enumaratie)
data BinaryDigit = Zero | One

-- tipuri de date generice:
data MyMaybe a = MyJust a | MyNothing
data List a = Cons a (List a) | Void
data Both a b = Both a b



bDigitOne :: BinaryDigit
bDigitOne = One

maybeFive :: MyMaybe Int
maybeFive = MyJust 5

myIntList :: List Int
myIntList = Cons 1 (Cons 2 Void) 


-- pattern matching pe tipurile definite cu `data`
listHead :: List a -> a
listHead (Cons x _) = x
listHead Void = undefined



{-
putem folosi `records` pt a da nume specifice parametrilor constructorului
-}


data Dog = Dog{ name :: String
              , breed :: String
              , age :: Int
              }



myDog :: Dog
myDog = Dog{name="Spike", breed="Golden Retriever", age = 5}




{- `record`

Sintaxa `record` poate fi folosita pentru tipuri cu mai multi constructor

In acest caz, constructorii pot imparti nume de parametri 
daca tipul parametrilor cu acelasi nume se potriveste
-}
data Expr = Atom Int | Add {left:: Expr, right:: Expr} | Subtract {left:: Expr, right:: Expr}
-- | ... -- here, 'left' from 'Add' needs to have the same type as 'left' from 'Subtract'



{- `newtype`

Un alt mod, mai specializat de a crea noi tipuri de date este
cuvantul cheie `newtype`.

Este folosit pentru a crea tipuri de date cu un singur constructor,
cu un singur parametru, cu o anumita utilizare a memoriei si optimizari de acces.
-}
newtype WithIndex a = WithIndex (a, Int)


data A = A Int
newtype B = B Int

{- `data` vs `newtype`
`data` -> constructorii sunt implementati ca un set de pointeri
catre parametrii de baza, astfel incat un obiect de tip A este in esenta
un pointer catre un `Int`.

Accesarea `Int`-ului de baza necesita un aces indirect la memorie
si avem neovie de spatiu alocat atat pentru Int, cat si pentru pointer.


`newtype` este reprezentat direct de tipul de baza, ceea ce duce
la eliminarea accesului indirect la memorie si elimina cerinta
de memorie suplimentara pentru stocarea pointerului
-}


{- `type` class
Sunt folosite pentru a grupa tipurile care au un anumit comportament comun.
De exemplu, toate tipurile care poti fi convertite intr-un sir
si imprimate pe consola apartin clasei de tip `Show`
-}



class MyShow a where
    myShow :: a -> String




-- to enroll a type in the this class, we create an `instance` for it
instance MyShow BinaryDigit where
    myShow One = "1"
    myShow Zero = "0" 


{-`type` class
Ne permite sa impune restrictii pe tipurile parametrilor in definitiile de functii,
alte definitie de type class si instante
-}


-- this function can show any table-like list-of-lists as long as the type of the elements is showable itself
showTable :: Show a => [[a]] -> String
showTable talbe = undefined  -- we can freely use 'show' one eahc cell of the table


class (Eq a) => Ord a where -- any type 'a' belonging to this class also needs to belog in the 'Eq' class
    (<), (<=), (>=), (>) :: a -> a -> Bool
    max, min             :: a -> a -> a


-- we define an instance for lists of all showable types
-- instance (Show a) => Show [a]
--     where
--         show = undefined



-- 10.1.1

-- the reuslt of a match could be a Win, Loss or Draw

-- cand tipul de date nu primeste valori, poate fi privite drept
-- un `enum` (enumeratie)
data MatchResult = Win | Loss | Draw
    deriving (Eq)

{- we care about 3 things:
* the player's name
* elo = floating point which measures the player's strength
* a list of results of past games (call it matchHistory)
-}
data Player = Player {
    playername :: String,                 -- numele jucatorului
    elo :: Float,                   -- elo (punctajul care masoara puterea jucatorului)
    matchHistory :: [MatchResult]   -- istoricul meciurilor jucate
} deriving (Eq)



-- `a` este un parmaetru de tip generic
data Tree a = Node a (Tree a) (Tree a) | Leaf a
    deriving (Show)

mirror :: Tree a -> Tree a
mirror (Leaf a) = Leaf a
mirror (Node a left right) = Node a (mirror right) (mirror left)




instance Show MatchResult where
    -- pattern matching asupra functiei `show`
    show Win = "A castigat"
    show Loss = "A pierdut"
    show Draw = "Remiza" 

instance Show Player where
    show player = playername player ++ "(Elo: " ++ show (elo player) ++ ", Istoric: " ++ show (matchHistory player) ++ ")"

addResult :: MatchResult -> Player -> Player
addResult result player = player { matchHistory = result : matchHistory player }


main :: IO ()
main = do
    putStrLn "Hello, world!"
