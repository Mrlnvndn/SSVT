import Data.List
import SetOrd
import System.Random
import Test.QuickCheck

type Rel a = [(a, a)]

isSerial :: (Eq a) => [a] -> Rel a -> Bool
isSerial domain relation = all (\e -> any (\(x, _) -> x == e) relation) domain

prop_nonEmpty :: (Eq a) => [a] -> Rel a -> Property
prop_nonEmpty domain relation = True ==> if not (null domain) then not (null relation) else null relation

prop_isSerial :: (Eq a) => [a] -> Rel a -> Property
prop_isSerial domain relation = True ==> all (\x -> any (\y -> (x, y) `elem` relation) domain) domain == isSerial domain relation

genDomain :: Gen [Integer]
genDomain = do
  domainsize <- choose (0, 10)
  return $ take domainsize [0 .. 9]

genRelation :: (Eq a) => [a] -> Gen (Rel a)
genRelation [] = return []
genRelation domain = do
  relationSize <- choose (1, 10)
  vectorOf relationSize (genTuple domain)

genTuple :: (Eq a) => [a] -> Gen (a, a)
genTuple domain = do
  index1 <- choose (0, length domain - 1)
  index2 <- choose (0, length domain - 1)
  return (domain !! index1, domain !! index2)

genCongruenceRelationModuloN :: [Integer] -> Gen (Rel Integer)
genCongruenceRelationModuloN domain = do
  n <- choose (1, 10)
  return [(x, y) | x <- domain, y <- domain, x `mod` n == y `mod` n]

main :: IO ()
main = do
  quickCheck $ forAll genDomain $ \domain -> forAll (genRelation domain) $ \relation -> prop_isSerial domain relation
  quickCheck $ forAll genDomain $ \domain -> forAll (genRelation domain) $ \relation -> prop_nonEmpty domain relation
  quickCheck $ forAll genDomain $ \domain -> forAll (genCongruenceRelationModuloN domain) $ \relation -> prop_isSerial domain relation
