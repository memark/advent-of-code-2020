import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!), Map)
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
-- \3 A, 1 B => 1 FUEL"makeChem' :: [R] -> Map C Q -> C -> Q

type C = String
type Q = Int
type I = (Q, C)
type O = (Q, C)
type R = ([I], O)
type T = Map Q C

type RS = Map C (Q, Map C Q)

main = do
  putStrLn ""
  input <- readFile "input.txt"

  -- let d = parse sample_1a
  -- putStrLn . List.intercalate "\n" . map show $ d
  -- putStrLn ""

  -- print $ makeChem d "FUEL"
  -- putStrLn ""

  let d' = parse' sample_1a
  print d'
  putStrLn ""

  -- Part One -- 

  -- Part Two -- 

  putStr ""



makeChem' :: RS -> T -> C -> Q
makeChem' _ _ "ORE" = 1
makeChem' rs t c =
  let
    (q,is) = rs ! c
    f = foldl ff ' is 
    ff acc x = 
  in
    1


parse' :: String -> Map C (Q, [(C, Q)])
parse' = Map.fromList . map parseLine . lines . trim
  where 
    parseLine :: String -> (C, (Q, [(C, Q)]))
    parseLine = parseParts . toTuple . splitOn " => "

    parseParts :: (String, String) -> (C, (Q, [(C, Q)]))
    parseParts (is, o) = (fst po, (snd po, pis))
      where pis = parseInputs is
            po = parseNumChem o

    parseInputs :: String -> [(C, Q)]
    parseInputs a = map parseNumChem . splitOn ", " $ a

    parseNumChem :: String -> (C, Q)
    parseNumChem b = case splitOn " " b of [q,c] -> (c, read q :: Int)


makeChem d "ORE" = 1
makeChem d chem = sum $ map (\ i -> fst i * makeChem d (snd i) `div` oq) is
  where 
    (oq,oc) = o
    (is,o) = r
    r = findR d chem


findR :: [R] -> C -> R
findR d oc =
  case f d of Just c -> c
              Nothing -> error (show oc ++ " was not found")
    where f = List.find (\ (is,o) -> snd o == oc)


parse :: String-> [([I], O)]
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