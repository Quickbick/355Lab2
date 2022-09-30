-- CptS 355 - Lab 2 (Haskell) - Fall 2022
-- Name: Nathanael Ostheller


module Lab2
     where


-- 1
{- (a) merge2 -}
merge2 :: [a] -> [a] -> [a]
merge2 [] [] = []
merge2 (x:xs) [] = (x:xs)
merge2 [] (y:ys) = (y:ys)
merge2 (x:xs) (y:ys) = x:y: (merge2 xs ys)
                         

{- (b) merge2Tail -}
merge2Tail :: [a] -> [a] -> [a]
merge2Tail [] [] = []
merge2Tail (x:xs) [] = (x:xs)
merge2Tail [] (y:ys) = (y:ys)
merge2Tail (x:xs) (y:ys) = reverse(MergeHelp (x:xs) (y:ys) [])
     where
          MergeHelp [] [] (zs) = (zs)
          MergeHelp (x:xs) [] (zs) = x:xs:zs
          MergeHelp [] (y:ys) (zs) = y:ys:zs
          MergeHelp (x:xs) (y:ys) (zs) = revMerge xs ys 


{- (c) mergeN -}




-- 2
{- (a) count -}




{- (b) histogram  -}




-- 3                
{- (a) concatAll -}




{- (b) concat2Either -}               
data AnEither  = AString String | AnInt Int
                deriving (Show, Read, Eq)



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






