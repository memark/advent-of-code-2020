import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Control.Exception as Ex
import qualified Control.Monad
import Text.Printf ()
import Data.Tuple.Select ()
import Data.List.Split (splitOn)


sample_1a = "\
\10 ORE => 10 A\n\
\1 ORE => 1 B\n\
\7 A, 1 B => 1 C\n\
\7 A, 1 C => 1 D\n\
\7 A, 1 D => 1 E\n\
\7 A, 1 E => 1 FUEL"

sample_1 = "\
\2 ORE => 3 A\n\
\6 A => 1 FUEL"

-- \3 ORE => 1 B\n\
-- \3 A, 1 B => 1 FUEL"

type Chemical = String
type Quantity = Int
type Input = (Quantity, Chemical)
type Output = (Quantity, Chemical)
type Reaction = ([Input], Output)

main = do
  putStrLn ""
  input <- readFile "input.txt"

  let d = parse sample_1a
  putStrLn . List.intercalate "\n" . map show $ d
  putStrLn ""

  print $ makeChem d "FUEL"

  -- Part One -- 

  -- Part Two -- 

  putStr ""

makeChem d "ORE" = 1
makeChem d chem = sum $ map (\ i -> fst i * makeChem d (snd i) `div` x) (fst r)
  where 
    x = fst (snd r)
    r = findReaction d chem


findReaction :: [Reaction] -> Chemical -> Reaction
findReaction d oc =
  case f d of Just c -> c
              Nothing -> error (show oc ++ " was not found")
    where f = List.find (\ (is,o) -> snd o == oc)


parse :: String-> [([Input], Output)]
parse = map parseLine . lines . trim
  where parseLine = parseParts . toTuple . splitOn " => "
        parseParts (inputs, output) = (parseInputs inputs, parseNumChem output)
        parseInputs a = map parseNumChem . splitOn ", " $ a
        parseNumChem b = case splitOn " " b of [b1,b2] -> (read b1 :: Int,b2)

toTuple [a,b] = (a,b)


-- splitBy _ [] = []
-- splitBy c s  =
--   let
--     i = (length . takeWhile (/= c)) s
--     (as, bs) = splitAt i s
--   in as : splitBy c (if null bs then [] else tail bs)


trim = List.dropWhileEnd Char.isSpace . dropWhile Char.isSpace