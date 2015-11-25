import Data.List

--Exercici 1
class Point p where
    sel:: Int -> p -> Double--done
    dim::p ->Int--done
    child::p -> p ->[Int] -> Int
    dist::p -> p -> Double--done
    list2Point:: [Double] -> p--done

--Exercici 2
data Point3d = Point3d Double Double Double deriving(Eq)

instance Show Point3d where
  show a = imprim a
    where
      imprim::Point3d -> String
      imprim (Point3d a b c) = "("++show a++"," ++ show b++"," ++ show c++")"

instance Point Point3d where

    sel n (Point3d a b c)
        | n==1 = a
        | n==2 = b
        | otherwise = c

    dim p = 3
    
    child p q list = bin2dec $ childA p q list
      where 
    --Donats dos punts, retorna el numero del fill, en binari
	    childA p q [] = []
	    childA p q (x:xs)
	        | (sel x q) <= (sel x p) = [0] ++ (childA p q xs)
	        | otherwise = [1] ++ (childA p q xs)
    --Donat un numero binari, retorna el seu decimal
	    bin2dec:: [Int] -> Int
	    bin2dec xs = sum (map (2^) ( findIndices (==1) $ reverse xs))
    
    dist (Point3d x1 y1 z1) (Point3d x2 y2 z2) = sqrt((x2-x1)^ 2+(y2-y1)^2+(z2-z1)^2)

    list2Point [a,b,c] = Point3d a b c
--Exercici 3    

data Kd2nTree p = Node p [Int] [Kd2nTree p] | Empty

instance Eq p => Eq (Kd2nTree p) where
  (==) Empty Empty = True
  (==) _ Empty = False
  (==) Empty _ = False
  {-(==) a1 a2 = equals a1 a2
    where 
    --igualtat de conjunts !!!-}
      
instance (Show p,Point p) => Show (Kd2nTree p) where
  show t = imprimir t
    where
      imprimir::(Show p,Point p) => Kd2nTree p ->String
      --fulla
      imprimir (Node a fill []) = impNode (Node a fill [])
      --altres
      imprimir a = show (impNode a)++ show (impFills a)
      impNode::(Show p,Point p) => Kd2nTree p -> String
      impNode (Node a fill _) = (show a) ++ " " ++ (show fill)++("\n")
      impFills::(Show p,Point p) => Kd2nTree p -> String
      impFills (Node _ _ []) = ""
      impFills (Node a fill (Empty:xs)) = impFills (Node a fill xs)
      impFills (Node a fill ((Node x f ys):xs)) = "<"++show (child a x fill)++">"++(impNode (Node x f ys))++"\n"
                                                    ++(show (impFills (Node x f ys)))++
                                                    (show (impFills(Node a fill xs)))