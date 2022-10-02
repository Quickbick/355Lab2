-- CptS 355 - Lab 2 (Haskell) - Fall 2022
-- Name: Nathanael Ostheller


module Lab2
     where

revappend [] list = list
revappend (x:xs) list = (revappend xs (x:list))

eliminateDuplicates xs = foldr elimHelp [] xs
     where 
          elimHelp x base | (x `elem` base) = base
                          | otherwise = x:base

-- 1
{- (a) merge2 -}
merge2 :: [a] -> [a] -> [a]
merge2 [] [] = []
merge2 xs [] = xs
merge2 [] ys = ys
merge2 (x:xs) (y:ys) = x:y: (merge2 xs ys)            

{- (b) merge2Tail -}
merge2Tail :: [a] -> [a] -> [a]
merge2Tail [] [] = []
merge2Tail xs [] = xs
merge2Tail [] ys = ys
merge2Tail (x:xs) (y:ys) = reverse(mergeHelp (x:xs) (y:ys) [])
     where
          mergeHelp [] [] zs = zs
          mergeHelp xs [] zs = revappend xs zs
          mergeHelp [] ys zs = revappend ys zs
          mergeHelp (x:xs) (y:ys) (zs) = mergeHelp xs ys (y:x:zs)

{- (c) mergeN -}
--has non-exhaustive patterns
mergeN:: [[a]] -> [a]
mergeN [] = []
mergeN xs = foldl merge2 [] xs

-- 2
{- (a) count -}
count :: Eq a => a -> [a] -> Int
count n [] = 0
count n (x:xs) = length(filter eqs (x:xs))
     where 
          eqs x = x == n

{- (b) histogram  -}
histogram :: Eq a => [a] -> [(a, Int)]
histogram [] = []
histogram xs = eliminateDuplicates(map makeTuple xs)
     where 
          makeTuple x = (x, (count x xs))

-- 3                
{- (a) concatAll -}
concatAll ::  [[String]] -> String
concatAll [] = []
concatAll xs = concatList(map concatList xs)
     where
          concatList xs = foldl (++) [] xs

{- (b) concat2Either -}               
data AnEither  = AString String | AnInt Int
                deriving (Show, Read, Eq)

concat2Either:: [[AnEither]] -> AnEither
concat2Either xs = eitherListConcat(map eitherListConcat xs)
     where
          eitherListConcat xs = foldl eitherConcat (AString ("")) xs
               where
                    eitherConcat (AnInt x) (AnInt y) = AString ((show x) ++ (show y))
                    eitherConcat (AString xs) (AnInt y) = AString (xs ++ (show y))
                    eitherConcat (AnInt x) (AString ys) = AString ((show x) ++ ys)
                    eitherConcat (AString xs) (AString ys) = AString (xs ++ ys)

-- 4      
{-  concat2Str -}               




data Op = Add | Sub | Mul | Pow
          deriving (Show, Read, Eq)

evaluate:: Op -> Int -> Int -> Int
evaluate Add x y =  x+y
evaluate Sub   x y =  x-y
evaluate Mul x y =  x*y
evaluate Pow x y = x^y

data ExprTree a = ELEAF a | ENODE Op (ExprTree a) (ExprTree a)
                  deriving (Show, Read, Eq)

-- 5 
{- evaluateTree -}



-- 6
{- printInfix -}



--7
{- createRTree -}
data ResultTree a  = RLEAF a | RNODE a (ResultTree a) (ResultTree a)
                     deriving (Show, Read, Eq)






