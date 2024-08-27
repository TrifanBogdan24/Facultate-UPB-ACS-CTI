import Data.List (sortOn)
import Data.Ord (Down(..))
import qualified Data.Map as Map
import Data.Map (Map)

-- Aliasuri de tipuri
type Matrix = [[Int]]
type Name = String
type PhoneNumber = String
type PhoneBook = Map Name PhoneNumber

-- Definirea tipurilor de date
data MatchResult = Win | Loss | Draw deriving (Eq, Show)

data Player = Player {
    name :: String,                 -- numele jucatorului
    elo :: Float,                   -- elo (punctajul care masoara puterea jucatorului)
    matchHistory :: [MatchResult]   -- istoricul meciurilor jucate
} deriving (Eq)

data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving (Show)

-- Instanta Show pentru Player
instance Show Player where
    show player = name player ++ " (Elo: " ++ show (elo player) ++ ", Istoric: " ++ show (matchHistory player) ++ ")"

-- Actualizare istoric meciuri pentru un jucator
addResult :: MatchResult -> Player -> Player
addResult result player = player { matchHistory = result : matchHistory player }

-- Puncte pentru rezultatele meciurilor
points :: MatchResult -> Float
points Win = 1
points Loss = 0
points Draw = 0.5

-- Scorul unui jucator
score :: Player -> Float
score player = sum (map points (matchHistory player))

-- Relatii de ordine pentru jucatori bazate pe scor
instance Ord Player where
  (<=) :: Player -> Player -> Bool
  p1 <= p2 = score p1 <= score p2

-- TODO 10.1.7: Simularea unui meci intre doi jucatori bazata pe elo
playGame :: Player -> Player -> (Player, Player)
playGame p1 p2
    | elo p1 > elo p2 = (addResult Win p1, addResult Loss p2)
    | elo p1 < elo p2 = (addResult Loss p1, addResult Win p2)
    | otherwise       = (addResult Draw p1, addResult Draw p2)

-- TODO 10.1.7: Simularea unui meci cu câștigator
playGameWithWinner :: Player -> Player -> (Player, (Player, Player))
playGameWithWinner p1 p2
    | elo p1 >= elo p2 = (p1', (p1', p2'))
    | otherwise        = (p2', (p1', p2'))
  where
    (p1', p2') = playGame p1 p2

-- TODO 10.1.8: Simularea unui meci intre un jucator și o lista de jucatori
playAll :: Player -> [Player] -> (Player, [Player])
playAll player [] = (player, [])
playAll player (p:ps) =
    let (player', p') = playGame player p
        (player'', ps') = playAll player' ps
    in (player'', p':ps')

-- TODO 10.1.9: Simularea unei grupe
playGroup :: [Player] -> [Player]
playGroup [] = []
playGroup (p:ps) =
    let (p', ps') = playAll p ps
    in p' : playGroup ps'

-- TODO 10.1.9: Selectarea celor mai buni jucatori
selectPlayers :: [Player] -> Int -> ([Player], [Player])
selectPlayers players m =
    let sortedPlayers = sortOn Down players
    in splitAt m sortedPlayers

-- TODO 10.1.9: Simularea grupurilor și selectarea celor mai buni jucatori
playGroups :: [[Player]] -> Int -> ([Player], [Player])
playGroups groups m = foldr selectGroup ([], []) groups
  where
    selectGroup group (selected, rest) =
        let (selectedGroup, restGroup) = selectPlayers (playGroup group) m
        in (selectedGroup ++ selected, restGroup ++ rest)

-- TODO 10.1.10: Simularea fazei de eliminare
playElimination :: [Player] -> Tree Player
playElimination [p] = Leaf p
playElimination players =
    let pairs = pairUp players
        results = map (\(p1, p2) -> let (winner, (p1', p2')) = playGameWithWinner p1 p2 in (winner, Node winner (Leaf p1') (Leaf p2'))) pairs
    in playElimination (map fst results)
  where
    pairUp (x:y:xs) = (x, y) : pairUp xs
    pairUp _ = []

-- TODO 10.1.11: Rezultatele fazei de eliminare sub forma de lista
eliminationResults :: Tree Player -> [Player]
eliminationResults (Leaf p) = [p]
eliminationResults (Node p l r) = p : (eliminationResults l ++ eliminationResults r)

-- TODO 10.1.12: Rularea intregului turneu
runTournament :: [Player] -> Int -> Int -> [Player]
runTournament players n m =
    let groups = splitIntoGroups players n
        (selectedPlayers, _) = playGroups groups m
        eliminationTree = playElimination selectedPlayers
    in sortOn (Down . score) (eliminationResults eliminationTree)

-- Helper pentru a imparti jucatorii in grupuri
splitIntoGroups :: [Player] -> Int -> [[Player]]
splitIntoGroups [] _ = []
splitIntoGroups players n =
    let (group, rest) = splitAt n players
    in group : splitIntoGroups rest n

-- Exemplu de jucatori (random)
players :: [Player]
players = [
    Player {name = "Jill Todd", elo = 69.32222, matchHistory = []},
    Player {name = "Cara Wong", elo = 68.451675, matchHistory = []},
    Player {name = "Travis Dunlap", elo = 49.667397, matchHistory = []},
    Player {name = "Adam Mills", elo = 65.36233, matchHistory = []},
    Player {name = "Josephine Barton", elo = 14.974056, matchHistory = []},
    Player {name = "Erica Mendez", elo = 27.466717, matchHistory = []},
    Player {name = "Derrick Simmons", elo = 11.790775, matchHistory = []},
    Player {name = "Paula Hatch", elo = 80.039635, matchHistory = []},
    Player {name = "Patricia Powers", elo = 61.08892, matchHistory = []},
    Player {name = "Luke Neal", elo = 65.933014, matchHistory = []},
    Player {name = "Jackie Stephenson", elo = 86.00121, matchHistory = []},
    Player {name = "Bernice Nixon", elo = 2.8692048, matchHistory = []},
    Player {name = "Brent Cobb", elo = 39.80139, matchHistory = []},
    Player {name = "Bobbie Sanderson", elo = 81.07552, matchHistory = []},
    Player {name = "Zachary Conner", elo = 63.88572, matchHistory = []},
    Player {name = "Shawn Landry", elo = 7.68082, matchHistory = []},
    Player {name = "Mabel Gentry", elo = 88.13421, matchHistory = []},
    Player {name = "Enrique Ali", elo = 9.568502, matchHistory = []},
    Player {name = "Clara McLaughlin", elo = 60.83427, matchHistory = []},
    Player {name = "Jacqueline Connell", elo = 60.091232, matchHistory = []},
    Player {name = "Jared Morgan", elo = 49.84152, matchHistory = []},
    Player {name = "Lorraine Castaneda", elo = 34.701054, matchHistory = []},
    Player {name = "Robin Hurd", elo = 78.33226, matchHistory = []},
    Player {name = "Vince Dunlap", elo = 63.634525, matchHistory = []},
    Player {name = "Elaine Winter", elo = 34.86934, matchHistory = []},
    Player {name = "Bennie Godfrey", elo = 73.81608, matchHistory = []},
    Player {name = "Gale Britton", elo = 16.05768, matchHistory = []},
    Player {name = "Jeanne Mathis", elo = 34.603416, matchHistory = []},
    Player {name = "Aida Greenwood", elo = 8.308169, matchHistory = []},
    Player {name = "Christian Witt", elo = 80.397675, matchHistory = []},
    Player {name = "Cecelia Dyer", elo = 80.657974, matchHistory = []},
    Player {name = "Edwin Gallagher", elo = 14.976497, matchHistory = []}
    ]


-- Exemplu de jucatori
myPlayers :: [Player]
myPlayers = [
    Player {name = "Alice Johnson", elo = 72.5, matchHistory = [Win, Loss, Win]},
    Player {name = "Bob Smith", elo = 65.3, matchHistory = [Loss, Loss, Win]},
    Player {name = "Charlie Brown", elo = 50.2, matchHistory = [Draw, Win, Loss]},
    Player {name = "Diana Prince", elo = 80.1, matchHistory = [Win, Win, Win]},
    Player {name = "Eve Adams", elo = 60.4, matchHistory = [Loss, Draw, Win]},
    Player {name = "Frank Wright", elo = 55.6, matchHistory = [Win, Loss, Loss]},
    Player {name = "Grace Hopper", elo = 85.0, matchHistory = [Win, Win, Loss]},
    Player {name = "Hank Hill", elo = 40.7, matchHistory = [Loss, Draw, Loss]}
    ]


-- Main function
main :: IO ()
main = do
    let scorFinal = runTournament players 8 4
    putStrLn "Clasamentul final pentru primul grup de jucatori:"
    mapM_ print scorFinal

    putStrLn ""     -- linie noua la stdout

    let myPlayersScore = runTournament myPlayers 4 2
    putStrLn "Clasamentul final pentru al doilea grup de jucatori:"
    mapM_ print myPlayersScore
