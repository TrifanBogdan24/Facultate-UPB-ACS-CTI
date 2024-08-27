module Lambda where

import Data.List (nub, (\\), sort)
import Language.Haskell.TH.Syntax (sequenceQ)

data Lambda = Var String                -- x
            | App Lambda Lambda         -- (f x)
            | Abs String Lambda         -- λx.e
            | Macro String              -- I

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        -- elem [List] x
        -- return TRUE daca x se gaseste in lista, altfel FALSE
        eq (Var x) (Var y) (env, xb, yb) = elem (x, y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False



-- 1.1. (5p) Implementați funcția auxiliară vars care returnează o listă cu toate String-urile care reprezintă variabile într-o expresie.
-- 
-- Cazuri tratate:
-- 1. Daca expresia este formata dintr-o variabila, o va returna pe aceasta
-- 2. MACRO-urile se vor ignora
-- 3. Daca expresia este o APP (aplicatie) intre alte doua expresii, atunci se vor returna variabilele din ele, eliminand duplicatele
-- 4. Daca expresai este o ABS (abstractie) intre o variabila si o alta epxresie, atunci se ve returna variabila, alaturi de variabilelele expresiei, eliminand duplicatele
-- 
-- tip param 1 : `Labmda`
-- tip parma 2 : list `String`
vars :: Lambda -> [String]
vars (Var x) = [x]
vars (Macro _) = []
vars (App e1 e2) = nub (vars e1 ++ vars e2)
vars (Abs x e1) = nub (x : vars e1)



-- 1.2. (5p) Implementați funcția auxiliară freeVars care returnează o listă cu toate String-urile care reprezintă variabile libere într-o expresie.
-- 
-- Cazuri tratate:
-- 1. Daca expresia contine doar variabile, atunci toate acestea sunt libere
-- 2. Se ignora MACRO-urile
-- 3. Daca expresia este formata dintr-o APP (Aplicatie), atunci returnam variabilele care se ragasesc in ambele expresii
-- 4. Daca expresia este formata dintr-o ABS (Abstractie) dintre variabile si o expresie, atunci returnam variabilele care se gasesc in expresie, cu exceptia celor din variabilele abstractiei
--      PS: operatorul `\\` face diferenta dintre doua liste
--      PS: functia `nub` primeste o lista si returneaza toate elementele, fara duplicate
--
-- tip param 1 : `Labmda`
-- tip parma 2 : list `String`
freeVars :: Lambda -> [String]
freeVars (Var x) = [x]
freeVars (Macro _) = []
freeVars (App e1 e2) = nub (freeVars e1 ++ freeVars e2)
freeVars (Abs x e) = freeVars e \\ [x]

-- 1.3. (10p) Implementați funcția auxiliară newVar care primește o listă de String-uri și intoarce cel mai mic String lexicografic care nu apare în listă.
-- tip param 1: list `String`
-- tip return : String

newVar :: [String] -> String
newVar usedVars = case filter (`notElem` usedVars) candidate of
    (x:_) -> x
    []    -> error "No available variable names"
  
  where
    allVars :: Int -> [String]
    allVars len = sequence $ replicate len ['a'..'z']
    
    candidate :: [String]
    candidate = [var | len <- [1..], var <- allVars len]



-- 1.4. (5p) Implementați funcția isNormalForm care verifică daca o expresie este în formă normală.
--
-- forma normala = expresie ireductibila (nu poate fi simplificata mai mult)
--
-- Cazuri tratate
-- * 1. Varibielele simple (`λx`) si MACRO-urile sunt IN FORMA NORMALA
-- * 2. forma normala a unei ABS (Abstractii) cu si o expresie este forma normala a expresiei
-- * 3. daca avem o aplicatie cu o abstractie, atunci este caz REDEX (nu este in forma normala)
-- * 4. O APP (Aplicatie) cu doua expresie este in forma normala daca ambele expresii sunt in forma normala
--
-- tip param 1: `Lambda`
-- tip return : `Bool`
isNormalForm :: Lambda -> Bool
isNormalForm (Var _) = True
isNormalForm (Macro _) = True
isNormalForm (Abs x e) = isNormalForm e
isNormalForm (App (Abs x e1) e2) = False
isNormalForm (App e1 e2) = isNormalForm e1 && isNormalForm e2


-- 1.5. (20p) Implementați funcția reduce care realizează β-reducerea unui redex luând în considerare și coliziunile de nume.
-- tip param 1: `String` 
-- tip param 2: `Lambda`  (expresia asta ne interee)
-- tip param 3: `Lambda`
-- tip return : `Lambda`
reduce :: String -> Lambda -> Lambda -> Lambda

reduce x (Var y) e2
    | x == y = e2
    | otherwise = Var y

reduce x (App e1 e2) e2' =
    App (reduce x e1 e2') (reduce x e2 e2')

reduce x (Abs y e1) e2
    | x == y = Abs y e1
    | y `elem` freeVars e2 =
        let z = newVar ([y] ++ vars e1 ++ vars e2)
        in Abs z (reduce x (rename y z e1) e2)
    | otherwise = Abs y (reduce x e1 e2)
  where
    rename :: String -> String -> Lambda -> Lambda
    
    rename old new (Var y)
        | y == old = Var new
        | otherwise = Var y
    
    rename old new (App e1 e2) =
        App (rename old new e1) (rename old new e2)

    rename old new (App (Abs x e1) e2)
        | x == old = App e1 (rename old new e2) 
        | otherwise = 




    rename _ _ (Macro x) = Macro x

reduce _ (Macro x) _ = Macro x

-- 1.6. (10p) Implementați funcția normalStep care aplică un pas de reducere după strategia Normală.
-- reducerea celui mai EXTERIOR din STANGA REDEX
-- tip param 1: `Lambda`
-- tip return : `Lambda`
normalStep :: Lambda -> Lambda

normalStep (App (Abs x e1) e2) = reduce x e1 e2

normalStep (App e1 e2)
    | not (isNormalForm e1) = App (normalStep e1) e2
    | otherwise = App e1 (normalStep e2)
normalStep (Abs x e) = Abs x (normalStep e)

normalStep e = e

-- 1.7. (10p) Implementați funcția applicativeStep care aplică un pas de reducere după strategia Aplicativă.
-- reducerea celui mai intai din STANGA REDEX
-- tip param 1: `Lambda`
-- tip return : `Lambda`
applicativeStep :: Lambda -> Lambda

applicativeStep (App (Abs x e1) e2)
    | isNormalForm e2 = reduce x e1 e2
    | otherwise = App (Abs x e1) (applicativeStep e2)

applicativeStep (App e1 e2)
    | not (isNormalForm e1) = App (applicativeStep e1) e2
    | otherwise = App e1 (applicativeStep e2)

applicativeStep (Abs x e) = Abs x (applicativeStep e)

applicativeStep e = e

-- 1.8. (5p) Implementați funcția simplify, care primeste o funcție de step și o aplică până expresia rămâne în formă normală, și întoarce o listă cu toți pași intermediari ai reduceri.
-- 
-- 
-- 
-- tip param 1: `Lambda -> Lambda` (functie care primeste `Lambda` si returneaza `Lambda`)
-- tip param 2: `Lambda`
-- tip return : list `Lambda`
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify step e
    | isNormalForm e = [e]
    | otherwise = e : simplify step (step e)


