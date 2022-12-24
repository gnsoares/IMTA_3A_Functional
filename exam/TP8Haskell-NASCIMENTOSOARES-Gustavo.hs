
-- NOM : NASCIMENTO SOARES
-- Prenom : Gustavo

-- [REC] = the function calls itself
-- [NONREC] = the function does not call itself
-- [ZF] = comprehension list

-- you can only use : (++), (<), (==), (<=), (>), drop, div, foldr, length, take

-- balance

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show,Eq)

-- Q1) [REC] return the list of values from left to right
fringe :: Tree a -> [a]
fringe (Leaf x)   = [x]
fringe (Node l r) = fringe l ++ fringe r

test1A = fringe (Node (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) (Leaf 4)) == [1,2,3,4]

-- Q2) [REC] build a balanced tree with the values of the list
mkTree :: [a] -> Tree a
mkTree [x]      = Leaf x
mkTree xs = Node (mkTree (take mid xs)) (mkTree (drop mid xs))
    where mid = div (length xs) 2


test2A = mkTree [1,2,3,4] == Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))
test2B = mkTree [1,2,3,4,5] == Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Node (Leaf 4) (Leaf 5)))

balance :: Tree a -> Tree a
balance = mkTree . fringe

-- minimumBy

data MyOrdering = MyLT | MyEQ | MyGT deriving (Show,Eq)

-- Q3) [NOTREC] use a metric function to compare two arguments
myComparing :: Ord b => (a -> b) -> a -> a -> MyOrdering
myComparing f v w | f v == f w = MyEQ
                  | f v < f w = MyLT
                  | otherwise = MyGT

test3A = myComparing length "long" "short" == MyLT
test3B = myComparing length "long" "tiny"  == MyEQ
test3C = myComparing length "short" "tiny" == MyGT

-- Q4) [REC] use a comparator function to return the minimum of a list
myMinimumBy :: (a -> a -> MyOrdering) -> [a] -> a
myMinimumBy f [x] = x
myMinimumBy f (x1:x2:xs) | f x1 x2 == MyGT = myMinimumBy f (x2:xs)
                         | otherwise = myMinimumBy f (x1:xs)

test4A = myMinimumBy (myComparing last) ["short","long","tiny"] == "long"

-- merge sort

-- Q5) [REC] split a list into two lists
split :: [a] -> ([a],[a])
split []     = ([], [])
split [x]    = ([x], [])
split (x1:x2:xs) = (x1:xs1, x2:xs2)
    where (xs1, xs2) = split xs

test5A = split [1..9] == ([1,3,5,7,9],[2,4,6,8])

-- Q6) [REC] merge two ordered lists into a single ordered list
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge (x1:x1s) (x2:x2s) | x1 < x2 = x1:merge x1s (x2:x2s)
                        | otherwise = x2:merge (x1:x1s) x2s

test6A = merge [1,4,5,7] [2,10] == [1,2,4,5,7,10]

-- Q7) [REC] sort a list with merge
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort x1s) (mergeSort x2s)
    where (x1s, x2s) = split xs

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
pThen p1 p2 s = [(p1 ++ p2, tbp2) | (p1, tbp1) <- p1 s, (p2, tbp2) <- p2 tbp1]

test8A = (pChar 'a' `pThen` pChar 'b') "abcdef" == [("ab","cdef")]
test8B = null (((pChar 'a') `pThen` (pChar 'b')) "acdef")

-- Q9) [NOTREC] compose two parsers in parallel
pOr :: Parser -> Parser -> Parser
pOr p1 p2 s = p1 s ++ p2 s

test9A = (pChar 'a' `pOr` pChar 'b') "abcdef" == [("a","bcdef")]
test9B = (pChar 'a' `pOr` pChar 'b') "bacdef" == [("b","acdef")]
test9C = (pChar 'a' `pOr` (pChar 'b' `pThen` pChar 'a')) "bacdef" == [("ba","cdef")]
test9D = (pChar 'a' `pOr` (pChar 'b' `pThen` pChar 'a')) "abcdef" == [("a","bcdef")]
test9E = (pChar 'a' `pOr` (pChar 'a' `pThen` pChar 'b')) "abcdef" == [("a","bcdef"),("ab","cdef")]

-- parse a (possibly empty) sequence
pMany :: Parser -> Parser
pMany p = (p `pThen` pMany p) `pOr` pNothing

-- Q10) [NOTREC] parse a non empty sequence
pMany1 :: Parser -> Parser
pMany1 p s = (p `pThen` pMany1 p) s ++ p s

test10A = pMany1 (pChar 'a') "aaabcdef" == [("aaa","bcdef"),("aa","abcdef"),("a","aabcdef")]
test10B = null ((pMany1 (pChar 'a')) "bacdef")
test10C = pMany1 (pChar 'a' `pThen` pChar 'b') "ababacdef" == [("abab","acdef"),("ab","abacdef")]

testFinal = and [test1A, test2A, test2B, test3A, test3B, test3C, test4A, test5A, test6A, test7A, test8A, test8B, test9A, test9B, test9C, test9D, test9E, test10A, test10B, test10C]
