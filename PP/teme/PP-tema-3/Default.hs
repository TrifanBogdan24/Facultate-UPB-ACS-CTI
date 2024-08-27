module Default where

import Lambda
import Binding
import Data.ByteString.Lazy (LazyByteString)

-- Variables (for convenience)
vx = Var "x"
vy = Var "y"
vz = Var "z"
vf = Var "f"
vg = Var "g"
vh = Var "h"
vm = Var "m"
vn = Var "n"
vu = Var "u"

-- Basic combinators

m :: Lambda
m = Abs "x" $ App vx vx

i :: Lambda
i = Abs "x" $ vx

k :: Lambda
k = Abs "x" $ Abs "y" $ vx

ki :: Lambda
ki = Abs "x" $ Abs "y" $ vy

c :: Lambda
c = Abs "x" $ Abs "y" $ Abs "z" $ App (App vx vz) vy

y :: Lambda
y = Abs "f" $ App fix fix
  where fix = Abs "x" $ App vf (App vx vx)

-- 4.1. Boolean encodings

-- `TRUE` = λx.λy.x
-- <=> k
bTrue :: Lambda
bTrue = Abs "x" $ Abs "y" $ vx 

-- `FALSE` = λx.λy.y
--  <=> ki
bFalse :: Lambda
bFalse = Abs "x" $ Abs "y" $ vy

-- `AND` = λx.λy.((x y) FALSE)
-- daca vp este true returneaza vq altfel returneaza false
bAnd :: Lambda
bAnd = Abs "x" $ Abs "y" $ App (App vx vy) bFalse

-- `OR` = λx.λy.((x TRUE) y)
-- daca vp este true returneaza true/vp altfel returneaza vq
bOr :: Lambda
bOr = Abs "x" $ Abs "y" $ App (App vx bTrue) vy


-- `NOT` = λx.((x FALSE) TRUE)
-- <=> c
-- daca vp este true returneaza false altfel true
bNot :: Lambda
bNot = Abs "x" $ App (App vp bFalse) bTrue
  where vp = Var "x"

-- `XOR` = λx.λy.((x (NOT y)) y)
-- returneaza True daca ambele sunt diferite
bXor :: Lambda
bXor = Abs "x" $ Abs "y" $ App (App vx (App bNot vy)) vy

-- 4.2. Pair encodings

-- `PAIR` = λx.λy.λf.((f x) y)
-- aplica f pe x si pe y  (x y) -> (f(x), f(y))
pair :: Lambda
pair = Abs "x" $ Abs "y" $ Abs "f" $ App (App vf vx) vy
  where vf = Var "f"; vx = Var "x"; vy = Var "y"

-- `FIRST` = λf.(f λx.λy.x)
-- `FIRST` = λf.(f TRUE)
-- primul element (echivalent cu bTrue pe vx)
first :: Lambda
first = Abs "f" $ App vf (Abs "x" $ Abs "y" $ vx)

-- `SECOND` = λf.(f λx.λy.y)
-- `SECOND` = λf.(f FALSE)
-- al doilea element (echivalent cu bFalse pe vx)
second :: Lambda
second = Abs "f" $ App vf (Abs "x" $ Abs "y" $ vy)

-- 4.3. Natural number encodings

-- `N0` = λf.λx.x
-- combinatorul identitate (se returneaza pe el insusi)
n0 :: Lambda
n0 = Abs "f" $ Abs "x" $ vx

-- `N1` = λf.λx.(f x)
-- f(x)
n1 :: Lambda
n1 = Abs "f" $ Abs "x" $ App vf vx

-- `N2` = λf.λx.(f (f x))
-- f(f(x))
n2 :: Lambda
n2 = Abs "f" $ Abs "x" $ App vf (App vf vx)

-- `SUCC` = λn.λf.λx.(f ((n f) x))
-- pornind de la n0 aplica f de n ori, apoi mai aplica o data f, ca sa ajungem la x + 1
nSucc :: Lambda
nSucc = Abs "n" $ Abs "f" $ Abs "x" $ App vf (App (App vn vf) vx)

-- `PRED` = λn.λf.λx.((n λg.λh.(h (g f)) λu.x) λu.u)
nPred :: Lambda
nPred = Abs "n" $ Abs "f" $ Abs "x" $
        App (App (App (Var "n") (Abs "g" $ Abs "h" $ App vh (App vg vf)))
                 (Abs "u" $ vx)) 
            (Abs "u" $ vu)


-- `ADD` = λn.λm.λf.λx.((n f) ((m f) x))
-- aplica de m ori f incepand de la n0, pentru a obtine numarul M
-- aplica apoi de n ori functia f, pentru a obtine numarul N
nAdd :: Lambda
nAdd = Abs "m" $ Abs "n" $ Abs "f" $ Abs "x" $ App (App vm vf) (App (App vn vf) vx)

-- `SUB` = λm.λn.((n PRED) m)
-- aplica de n ori nPred pe vm
nSub :: Lambda
nSub = Abs "m" $ Abs "n" $ App (App (Var "n") nPred) (Var "m")

-- `MULT` = λm.λn.λf. (m (n f) x)
-- aplica de m (n * f) 
nMult = Abs "m" $ Abs "n" $ Abs "f" $ App vm (App vn vf)





-- Default Context
defaultContext :: Context
defaultContext = 
    [ ("M", m)
    , ("I", i)
    , ("K", k)
    , ("KI", ki)
    , ("C", c)
    , ("Y", y)
    , ("TRUE", bTrue)
    , ("FALSE", bFalse)
    , ("AND", bAnd)
    , ("OR", bOr)
    , ("NOT", bNot)
    , ("XOR", bXor)
    , ("PAIR", pair)
    , ("FST", first)
    , ("SND", second)
    , ("N0", n0)
    , ("N1", n1)
    , ("N2", n2)
    , ("SUCC", nSucc)
    , ("PRED", nPred)
    ,("ADD", nAdd)
    , ("SUB", nSub)
    , ("MULT", nMult)
    ]
