import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set as Set

sample_1a = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
sample_1b = "1102,34915192,34915192,7,4,7,99,0"
sample_1c = "104,1125899906842624,99"

main = do
  input <- readFile "input.txt"

  print (runProgram 0 0 [] [] . parse $ sample_1a)

  -- Part One -- 5346030
  -- print (runProgram 0 [1] [] . parse $ input)

  -- Part Two -- 513116
  -- print (runProgram 0 [5] [] . parse $ input)

  putStr ""

type Input = [Int]
type Output = [Int]
type Memory = [Int]
type RelBase = Int

runProgram :: Int -> RelBase -> Input -> Output -> Memory -> Output
runProgram ip rb inp outp mem
  | op == 01 = op_add
  | op == 02 = op_multiply
  | op == 03 = op_input
  | op == 04 = op_output
  | op == 05 = op_jump_if_true
  | op == 06 = op_jump_if_false
  | op == 07 = op_less_than
  | op == 08 = op_equals
  | op == 09 = op_rel_base_offs
  | op == 99 = outp

  where
    op_input         = runProgram (ip+1 + 1) rb         (tail inp)        outp  (update p1 (head inp) mem)
    op_output        = runProgram (ip+1 + 1) rb               inp  (pv1 : outp)                       mem
    jump nip         = runProgram nip        rb               inp         outp                        mem
    store val        = runProgram (ip+1 + 3) rb               inp         outp  (update p3 val        mem)

    op_rel_base_offs = runProgram (ip+1 + 1) (rb + pv1)       inp         outp                        mem

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
    -- pv1 = if p1m == 1 then p1 else mem !! p1 -- 2 relative mode
    -- pv2 = if p2m == 1 then p2 else mem !! p2

    pv1 =
      case p1m of 0 -> mem !! p1
                  1 -> p1
                  2 -> mem !! (rb + p1)
    pv2 =
      case p1m of 0 -> mem !! p2
                  1 -> p2
                  2 -> mem !! (rb + p2)
              
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
