import qualified Data.Char as Char
import qualified Data.List as List

sample_1a = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
sample_1b = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
sample_1c = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"

main = do
  input <- readFile "input.txt"

  -- Part One -- 116680
  print . head . maximum . map (eval (parse input) 0) . List.permutations $ [0..4]

  -- Part Two -- 

  
  putStr ""



eval :: Memory -> Int -> PhaseSettings -> Output
eval parsedData initialInput phaseSettings = outp5
  where outp5 = runProgram 0 inp5 [] parsedData
        inp5 = (phaseSettings !! 4):outp4
        outp4 = runProgram 0 inp4 [] parsedData
        inp4 = (phaseSettings !! 3):outp3
        outp3 = runProgram 0 inp3 [] parsedData
        inp3 = (phaseSettings !! 2):outp2
        outp2 = runProgram 0 inp2 [] parsedData
        inp2 = (phaseSettings !! 1):outp1
        outp1 = runProgram 0 inp1 [] parsedData
        inp1 = (phaseSettings !! 0):[initialInput]


type PhaseSettings = [Int]
type Input = [Int]
type Output = [Int]
type Memory = [Int]

runProgram :: Int -> Input -> Output -> Memory -> Output
runProgram ip inp outp mem
  | op == 01 = op_add
  | op == 02 = op_multiply
  | op == 03 = op_input
  | op == 04 = op_output
  | op == 05 = op_jump_if_true
  | op == 06 = op_jump_if_false
  | op == 07 = op_less_than
  | op == 08 = op_equals
  | op == 99 = outp

  where
    op_input         = runProgram (ip+1 + 1) (tail inp)        outp  (update p1 (head inp)  mem)
    op_output        = runProgram (ip+1 + 1)       inp  (pv1 : outp)                        mem
    jump nip         = runProgram nip              inp         outp                         mem
    store val        = runProgram (ip+1 + 3)       inp         outp  (update p3 val         mem)

    op_add           = store (pv1 + pv2)
    op_multiply      = store (pv1 * pv2)
    op_less_than     = store (if pv1 <  pv2 then 1 else 0)
    op_equals        = store (if pv1 == pv2 then 1 else 0)
    op_jump_if_true  = jump  (if pv1 /= 0 then pv2 else ip+1 + 2)
    op_jump_if_false = jump  (if pv1 == 0 then pv2 else ip+1 + 2)

    (op, p1m, p2m, p3m) = getModes $ mem !! ip
    p1  = mem !! (ip + 1)
    p2  = mem !! (ip + 2)
    p3  = mem !! (ip + 3)
    pv1 = if p1m == 1 then p1 else mem !! p1
    pv2 = if p2m == 1 then p2 else mem !! p2

getModes opx =
  ( opx `mod` 100,
    opx `div` 100 `mod` 10,
    opx `div` 1000 `mod` 10,
    opx `div` 10000 `mod` 10
  )

update n x a = take n a ++ [x] ++ drop (n + 1) a

parse = map toInt . splitBy ',' . trim

toInt x = read x :: Int

splitBy _ [] = []
splitBy c s =
  let i = (length . takeWhile (/= c)) s
      (as, bs) = splitAt i s
   in as : splitBy c (if null bs then [] else tail bs)

trim = List.dropWhileEnd Char.isSpace . dropWhile Char.isSpace
