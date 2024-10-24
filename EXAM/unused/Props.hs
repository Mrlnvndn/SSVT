import Data.List
import Data.Tuple
import Test.QuickCheck

type Rel a = [(a, a)]

-- =========================== INVERSE =================================================
    
inverse:: Eq a => Rel a -> Rel a
inverse = map swap 

-- =========================== COMPOSITION =================================================
-- composition relation {(x,z)∈X×Z | xRy and ySz for some y∈Y}. 

composeR :: Eq a => Rel a -> Rel a -> Int -> Rel a
composeR r1 r2 counter
  | counter == 1000 = r1
  | counter == 0 = composeR r1 newr2 (counter+1)
  | r1 == r2 = r1
  | otherwise = composeR r1 newr2 (counter+1)
  where newr2 = concatMap (\(x,y) -> concatMap (\(x2, y2) -> [(x,y2) | y == x2]) r1) r2

-- =========================== UNION=================================================

-- The union of R and S, written R∪S, is the relation {x(R∪S)y | xRy or xSy}.

unionRel :: (Ord a) => Rel a -> Rel a -> Rel a
unionRel [] rel2 = sort rel2
unionRel (x : xs) rel2 = sort $ insertRel x (unionRel xs rel2)

insertRel :: (Ord a) => (a, a) -> [(a, a)] -> [(a, a)]
insertRel = insertList

insertList :: Eq a => a -> [a] -> [a]
insertList x y
  | x `elem` y = y
  | otherwise = x : y


-- Union of R with R^{-1} -- def. of Symmetric Closure
symCloseUnion :: Ord a => Rel a -> Rel a
symCloseUnion r = unionRel r inverseOfR where inverseOfR = map swap r

-- Symmetric closure functionality
-- Referred to  below for symmetric closure definition:
--      https://math24.net/closures-relations.html
--      The Haskell Road to Logic, Math and Programming: Ch 5
symClos :: Ord a => Rel a -> Rel a
symClos []       = []
-- we use nub function to eliminate duplicate pairs
-- (inverse of (x, x) is (x,x), hence the inverse is identical)
symClos [x     ] = nub (createSymmetricTuples x)
symClos (x : xs) = nub (createSymmetricTuples x ++ symClos xs)

-- for each tuple within binary relation, we add to our result list the tuple itself,
-- and its inverse (using swap function)
createSymmetricTuples :: (a, a) -> [(a, a)]
createSymmetricTuples a = a : [swap a]