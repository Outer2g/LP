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
  show Empty = ""
  show (Node a fill xs) = (show a)++ " " ++ show fill ++ "\n" ++ imprimirFills xs a fill
    where
    imprimirFills:: (Show p,Point p)=>[Kd2nTree p] ->p ->[Int] -> String
    imprimirFills [] _ _ = ""
    imprimirFills (Empty:xs) a fill = "x"++imprimirFills xs a fill
    imprimirFills ((Node a fill1 ys):xs) p fill = "\n<"++show (child p a fill)++">"++
                                                  show a ++ " " ++ show fill1++
                                                  imprimirFills ys a fill1 ++
                                                  imprimirFills xs p fill++"\n"

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
            | pos <0 =[]
            | pos ==0 = [Empty]
            | otherwise = [Empty]++fillEmpty (pos-1)

insertAt:: Int -> p -> [Int] ->[Kd2nTree p] -> [Kd2nTree p]
insertAt pos p fill (x:[]) = [(Node p fill [])]
insertAt pos p fill (x:xs)
    | length xs == pos+1 = (Node p fill []):xs
    | otherwise = [x]++insertAt pos p fill xs

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