import Data.List

--Exercici 1
class Point p where
    sel:: Int -> p -> Double--done
    dim::p ->Int--done
    child::p -> p ->[Int] -> Int --done
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

instance (Eq p,Point p) => Eq (Kd2nTree p) where
  (==) Empty Empty = True
  (==) _ Empty = False
  (==) Empty _ = False
  (==) p q
    | get_all p == get_all q = True
    | otherwise = False
      
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

--Exercici 4a
insertt::(Point p,Eq p)=> Kd2nTree p -> p -> [Int] -> Kd2nTree p
insertt Empty p fill = (Node p fill [])
insertt (Node a fill []) p xs = (Node a fill (fillIt nen p xs))
    where
        nen = child a p fill
insertt (Node a fill llista) p xs
    | length llista -1 >= nen && llista!!nen == Empty = (Node a fill (insertAt nen p xs llista))
    | length llista -1 >= nen && llista!!nen /= Empty = (Node a fill (insertOnList llista nen p xs))
    | otherwise = (Node a fill magia)
    where
        nen = child a p fill
        magia = (llista++(fillIt (length llista - nen) p xs))
        insertOnList::(Point p,Eq p)=>[Kd2nTree p] -> Int -> p -> [Int] ->[Kd2nTree p]
        insertOnList (x:xs) pos p fill
            | pos == 0 = [insertt x p fill]++xs
            | otherwise = [x]++insertOnList xs (pos-1) p fill

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

--Exercici 4b
build::(Point p, Eq p)=>[(p,[Int])] -> Kd2nTree p
build llista = buildAux $ reverse llista
    where
        {-Per tal de insertar el Kd2nTree en ordre, faig una funcio que em tracti el revessat
         de la llista d'entrada-}
        buildAux::(Point p,Eq p) =>[(p,[Int])] -> Kd2nTree p
        buildAux ((p,fill):[]) = insertt Empty p fill
        buildAux ((p,fill):xs) = insertt (buildAux xs) p fill
--Exercici 4c
buildIni::(Point p,Eq p)=>[([Double],[Int])] ->Kd2nTree p
buildIni llista = buildAux $ reverse llista
    where
        --mateixa situacio que amb build
        buildAux::(Point p, Eq p)=> [([Double],[Int])] -> Kd2nTree p
        buildAux ((p,fill):[]) = insertt Empty (list2Point p) fill
        buildAux ((p,fill):xs) = insertt (buildAux xs) (list2Point p) fill

--Exercici 5
get_all:: Point p =>Kd2nTree p -> [(p,[Int])]
get_all Empty = []
get_all (Node a fill []) = [(a,fill)]
get_all (Node a fill (x:xs)) = [(a,fill)]++get_all x++continueList xs
  where 
    continueList::Point p =>[Kd2nTree p]->[(p,[Int])]
    continueList (x:[]) = get_all x
    continueList (x:xs) = get_all x++continueList xs
--Exercici 6
{-remove:: (Point p,Eq p)=>Kd2nTree p -> p -> Kd2nTree p
remove Empty p = Empty
remove (Node a fill []) p
    | p == a = Empty
    | otherwise = (Node a fill [])
remove (Node a fill (x:xs)) p
    | p == a = reBuildTree x xs
    | otherwise = map $ remove (x:xs)
    where
        reBuildTree::Kd2nTree p -> [Kd2nTree p] ->Kd2nTree
-}

--Exercici 7
contains::Eq p => Kd2nTree p -> p-> Bool
contains (Node a fill []) b = a==b
contains (Node a fill (x:xs)) b 
  | b == a = True
  | otherwise = contains (Node a fill xs) b || contains x b
--Exercici 8
{-}nearest:: Eq p =>Kd2nTree ->p -> p
nearest Empty p =5000
nearest (Node a fill []) p = a
nearest (Node a fill xs) p
    | dist a p < distFills xs p = a
    | otherwise = distFills xs p
    where
        distFills::[Kd2nTree p]-> p -> p
        distFills (x:[]) = nearest x p
        distFills (x:xs) = nearest
-}
--Exercici 9
allinInterval:: Ord p =>Kd2nTree p -> p -> p -> [p]
allinInterval (Node a fill []) b c
    | b<=a && a<=c = [a]
    | otherwise = []
allinInterval (Node a fill (x:xs)) b c
    | b<=a && a<=c = [a]++allinInterval x b c++allinInterval (Node a fill xs) b c
    | otherwise = allinInterval x b c ++ allinInterval (Node a fill xs) b c