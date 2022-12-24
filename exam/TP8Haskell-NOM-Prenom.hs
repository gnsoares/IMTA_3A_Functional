
-- NOM :
-- Prenom :

-- [REC] = the function calls itself
-- [NONREC] = the function does not call itself
-- [ZF] = comprehension list

-- you can only use : (++), (<), (==), (<=), (>), drop, div, foldr, length, take

-- balance

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show,Eq)

-- Q1) [REC] return the list of values from left to right
fringe :: Tree a -> [a]
fringe = undefined

test1A = fringe (Node (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) (Leaf 4)) == [1,2,3,4]

-- Q2) [REC] build a balanced tree with the values of the list
mkTree :: [a] -> Tree a
mkTree = undefined

test2A = mkTree [1,2,3,4] == Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))
test2B = mkTree [1,2,3,4,5] == Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Node (Leaf 4) (Leaf 5)))

balance :: Tree a -> Tree a
balance = mkTree . fringe

-- minimumBy

data MyOrdering = MyLT | MyEQ | MyGT deriving (Show,Eq)

-- Q3) [NOTREC] use a metric function to compare two arguments
myComparing :: Ord b => (a -> b) -> a -> a -> MyOrdering
myComparing = undefined

test3A = myComparing length "long" "short" == MyLT
test3B = myComparing length "long" "tiny"  == MyEQ
test3C = myComparing length "short" "tiny" == MyGT

-- Q4) [REC] use a comparator function to return the minimum of a list
myMinimumBy :: (a -> a -> MyOrdering) -> [a] -> a
myMinimumBy = undefined

test4A = myMinimumBy (myComparing last) ["short","long","tiny"] == "long"

-- merge sort

-- Q5) [REC] split a list into two lists
split :: [a] -> ([a],[a])
split = undefined

test5A = split [1..9] == ([1,3,5,7,9],[2,4,6,8])

-- Q6) [REC] merge two ordered lists into a single ordered list
merge :: Ord a => [a] -> [a] -> [a]
merge = undefined

test6A = merge [1,4,5,7] [2,10] == [1,2,4,5,7,10]

-- Q7) [REC] sort a list with merge
mergeSort :: Ord a => [a] -> [a]
mergeSort = undefined

test7A = mergeSort [5,4,7,4,10,1] == [1,4,4,5,7,10]

-- parse regular expressions

-- a parser progresses in a String

type ToBeParsed = String

type Parsed = String

type Parser = ToBeParsed -> [(Parsed,ToBeParsed)]

-- parse no char
pNothing :: Parser
pNothing s = [("",s)]

-- parse the next char if it is equal to c'
pChar :: Char -> Parser
pChar c' (c:cs) | c==c' = [([c],cs)]
pChar c' _              = []

-- Q8) [ZF] compose two parsers in sequence
pThen :: Parser -> Parser -> Parser
pThen p1 p2 s = undefined

test8A = ((pChar 'a') `pThen` (pChar 'b')) "abcdef" == [("ab","cdef")]
test8B = ((pChar 'a') `pThen` (pChar 'b')) "acdef"  == []

-- Q9) [NOTREC] compose two parsers in parallel
pOr :: Parser -> Parser -> Parser
pOr = undefined

test9A = ((pChar 'a') `pOr` (pChar 'b')) "abcdef" == [("a","bcdef")]
test9B = ((pChar 'a') `pOr` (pChar 'b')) "bacdef" == [("b","acdef")]
test9C = ((pChar 'a') `pOr` (pChar 'b' `pThen` pChar 'a')) "bacdef" == [("ba","cdef")]
test9D = ((pChar 'a') `pOr` (pChar 'b' `pThen` pChar 'a')) "abcdef" == [("a","bcdef")]
test9E = ((pChar 'a') `pOr` (pChar 'a' `pThen` pChar 'b')) "abcdef" == [("a","bcdef"),("ab","cdef")]

-- parse a (possibly empty) sequence
pMany :: Parser -> Parser
pMany p = (p `pThen` (pMany p)) `pOr` pNothing

-- Q10) [NOTREC] parse a non empty sequence
pMany1 :: Parser -> Parser
pMany1 = undefined

test10A = (pMany1 (pChar 'a')) "aaabcdef" == [("aaa","bcdef"),("aa","abcdef"),("a","aabcdef")]
test10B = (pMany1 (pChar 'a')) "bacdef" == []
test10C = (pMany1 (pChar 'a' `pThen` pChar 'b')) "ababacdef" == [("abab","acdef"),("ab","abacdef")]
