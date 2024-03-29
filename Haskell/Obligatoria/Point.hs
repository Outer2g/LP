import Data.List

--Exercici 1
class Point p where
    sel:: Int -> p -> Double--done
    dim::p ->Int--done
    child::p -> p ->[Int] -> Int --done
    dist::p -> p -> Double--done
    list2Point:: [Double] -> p--done
    dist2::p -> p-> Double -- Millora eficiencia (Consultar funcio nearest)
    ptrans::[Double] -> p ->p
    pscale::Double -> p -> p

--Exercici 2
data Point3d = Point3d Double Double Double deriving(Eq,Ord)

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

    dist2(Point3d x1 y1 z1) (Point3d x2 y2 z2) =(x2-x1)^ 2+(y2-y1)^2+(z2-z1)^2

    list2Point [a,b,c] = Point3d a b c

    ptrans [a,b,c] (Point3d x y z) = (Point3d (x+a) (b+y) (c+z))

    pscale k (Point3d x y z) = (Point3d (x*k) (y*k) (z*k))

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
  show (Node a fill xs) = (show a)++ " " ++ show fill ++ imprimirFills xs a fill 1
    where
    imprimirFills:: (Show p,Point p)=>[Kd2nTree p] ->p ->[Int] -> Int-> String
    imprimirFills [] _ _ _ = ""
    imprimirFills (Empty:xs) a fill nivell = imprimirFills xs a fill nivell
    imprimirFills ((Node a fill1 ys):xs) p fill nivell = "\n"++take (nivell*4) (cycle " ")++
                                                    "<"++show (child p a fill)++">"++
                                                  show a ++ " " ++ show fill1++
                                                  imprimirFills ys a fill1 (nivell+1)++
                                                  imprimirFills xs p fill nivell

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
remove:: (Point p,Eq p)=>Kd2nTree p -> p -> Kd2nTree p
remove Empty p = Empty
remove t@(Node a fill []) p
    | p == a = Empty
    | otherwise = t
remove t@(Node a fill list) p
    | p == a = build fills
    | otherwise = (Node a fill (map (flip remove p) list ))
    where
    --si agafem tots els punts de t, el primer sera el node a eliminar, per aixo fem drop 1
    fills = drop 1 (get_all t)

--Exercici 7
contains::Eq p => Kd2nTree p -> p-> Bool
contains (Node a fill []) b = a==b
contains (Node a fill (Empty:xs)) b = contains (Node a fill xs) b
contains (Node a fill (x:xs)) b 
  | b == a = True
  | otherwise = contains (Node a fill xs) b || contains x b

--Exercici 8
{- mesura per tal d'incrementar l'eficiencia de la funcio nearest: en comptes de fer el
calcul amb la funcio dist (la cual utilitza la funcio sqrt, la qual es molt lenta),
comparare les distancies cuadrades-}
nearest::(Eq p,Point p) =>Kd2nTree p ->p -> p
nearest (Node a fill []) p = a
nearest (Node a fill (Empty:xs)) p = nearest (Node a fill xs) p
nearest (Node a fill ((Node b f ys):xs)) p
    | dist2 a p < dist2 (kidsMinD ((Node b f ys):xs) p) p = a
    | otherwise = kidsMinD ((Node b f ys):xs) p
    where
        kidsMinD::(Eq p, Point p)=>[Kd2nTree p] -> p -> p
        kidsMinD (Empty:xs) p = kidsMinD xs p
        kidsMinD ((Node a fill []):[]) p = a
        kidsMinD ((Node a fill xs):[]) p
            | dist2 a p < dist2 (kidsMinD xs p) p = a
            | otherwise = kidsMinD xs p
        kidsMinD (x:xs) p
            | dist2 (nearest x p) p < dist2 (kidsMinD xs p) p = nearest x p
            | otherwise = kidsMinD xs p

--Exercici 9
allinInterval::Ord p => Kd2nTree p -> p->p->[p]
allinInterval tree a b = sort $ allinIntervalAux tree a b
    where
    allinIntervalAux:: Ord p =>Kd2nTree p -> p -> p -> [p]
    allinIntervalAux (Node a fill (Empty:xs)) b c = allinInterval (Node a fill xs) b c
    allinIntervalAux (Node a fill []) b c
        | b<=a && a<=c = [a]
        | otherwise = []
    allinIntervalAux (Node a fill (x:xs)) b c
        | b<=a && a<=c = [a]++allinInterval x b c++allinInterval (Node a fill xs) b c
        | otherwise = allinInterval x b c ++ allinInterval (Node a fill xs) b c

--Exercici 10
kdmap::(Point p,Point q) => (p -> q) -> Kd2nTree p ->Kd2nTree q
kdmap f Empty = Empty
kdmap f (Node a fill xs) = (Node (f a) fill (map(kdmap f) xs))

translate::Point p => [Double] -> Kd2nTree p -> Kd2nTree p
translate list tree = kdmap (ptrans list) tree

scale::Point p=> Double -> Kd2nTree p-> Kd2nTree p
scale k tree = kdmap (pscale k) tree