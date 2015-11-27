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
    --igualtat de conjunts !!!
    -}
      
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
--Exercici 4
insertt::(Point p,Eq p)=> Kd2nTree p -> p -> [Int] -> Kd2nTree p
insertt Empty p fill = (Node p fill [])
insertt (Node a fill []) p xs = (Node a fill (fillIt nen p xs))
    where
        nen = child a p fill
insertt (Node a fill llista) p xs
    | length llista >= nen && (llista!!nen) == Empty = (Node a fill (insertAt nen p xs llista))
    | length llista >= nen && llista!!nen /= Empty = insertt (llista!!nen) p xs
    | otherwise = (Node a fill magia)
    where
        nen = child a p fill
        magia = (llista++(fillIt (length llista - nen) p xs))

fillIt:: Int -> p -> [Int] -> [Kd2nTree p]
fillIt pos p fill = fillEmpty (pos-1)++[(Node p fill [])]
    where
        fillEmpty:: Int -> [Kd2nTree p]
        fillEmpty pos
            | pos ==0 = [Empty]
            | otherwise = [Empty]++fillEmpty (pos-1)

insertAt:: Int -> p -> [Int] ->[Kd2nTree p] -> [Kd2nTree p]
insertAt pos p fill (x:xs)
    | length xs == pos = (Node p fill []):xs
    | otherwise = insertAt pos p fill xs

{-insert:: Kd2nTree p -> p -> [Int] -> Kd2nTree p
insert Empty p fill = (Node p fill [])
insert (Node a fill []) p xs =(Node a fill (itWillFit [] (child a p fill) p xs))
insert (Node a fill list) p xs
    | ((length list) >= nen) && ((list!!nen)== Empty)=(Node a fill (insertAt nen list))
    | ((length list) >= nen) && ((list!!nen)\= Empty)=insert (list!!nen) p xs
    | otherwise = (Node a fill (fitThat list p nen nen xs))
    where nen = child a p fill
fitThat:: [Kd2nTree p] -> p -> Int -> -> [Kd2nTree p]
fitThat list p pos aux fill
    | aux==pos = (fitThat list p pos (aux-1) fill)++[(Node p fill [])]
    | length list < aux

itWillFit::[Kd2nTree p] -> Int->p ->[Int]->[Kd2nTree p]
itWillFit [] pos p fill
    | pos == 0 = [(Node a fill [])]
    | otherwise = [Empty] ++ itWillFit [] (pos-1) p fill

-}
--Exercici 5
get_all:: Point p =>Kd2nTree p -> [(p,[Int])]
get_all Empty = []
get_all (Node a fill []) = [(a,fill)]
get_all (Node a fill (x:xs)) = [(a,fill)]++get_all x++continueList xs
  where 
    continueList::Point p =>[Kd2nTree p]->[(p,[Int])]
    continueList (x:[]) = get_all x
    continueList (x:xs) = get_all x++continueList xs
--Exercici 7
contains::Eq p => Kd2nTree p -> p-> Bool
contains (Node a fill []) b = a==b
contains (Node a fill (x:xs)) b 
  | b == a = True
  | otherwise = contains (Node a fill xs) b || contains x b
--Exercici 8
{-}nearest:: Kd2nTree p -> p -> p
nearest (Node a fill list) q = fst myListMax ([a,dist a q]++getMax list q)
    where
        getMax::[kd2nTree p] -> p ->[(p,Double)]
        getMax ((Node a fill []):[]) q = [a,(dist a q)]
        getMax ((Node a fill xs):[]) q = [a,(dist a q)] ++ getMax xs q
        getMax (Node a fill (x:xs)) q = [a,(dist a q)] ++ getMax (x:xs) q
        myListMax::[(p,Double)] -> (p,Double)
        myListMax (x:[]) = x
        myListMax (x:xs)
            |fst x > myListMax xs = x
            | otherwise = myListMax xs
-}
--Exercici 9
allinInterval:: Ord p =>Kd2nTree p -> p -> p -> [p]
allinInterval (Node a fill []) b c
    | b<=a && a<=c = [a]
    | otherwise = []
allinInterval (Node a fill (x:xs)) b c
    | b<=a && a<=c = [a]++allinInterval x b c++allinInterval (Node a fill xs) b c
    | otherwise = allinInterval x b c ++ allinInterval (Node a fill xs) b c