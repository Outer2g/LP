class Point p where
    sel:: Int -> p -> Double--done
    dim::p ->Int--done
    child::p -> p ->[Int] -> Int
    dist::p -> p -> Double--done
    list2Point:: [Double] -> p--done


data Point3d = Point3d Double Double Double deriving(Eq,Show)

instance Point Point3d where

    sel n (Point3d a b c)
        | n==1 = a
        | n==2 = b
        | otherwise = c

    dim p = 3
    --sent p el punt i q el punt a afegir per a  cada coord de la llista q<=p => 0
    child p q xs = bin2d (dropWhile aux (reverse (childA p q xs)))
    aux:: Int ->Bool
    aux x
      | x==0 =True
      | otherwise = False
    --retornem el valor binari del fill
    child::p -> p ->[Int] -> [Int]
    childA p q [n]
      | sel n q <= sel n p = [0]
      | otherwise = [1]
    childA p q (x:xs)
      | sel x q <= sel x p = 0++(child p q xs)
      | otherwise = 1++(child p q xs)
    --donat un num en binari, retorna el seu valor decimal
    bin2d:: [Int] -> Int
    bin2d [] = 0
    bin2d [x] = x
    bin2d (x:xs)
      | x == 0 = bin2d(xs)
      | otherwise = 2*bin2d(xs)
    dist (Point3d x1 y1 z1) (Point3d x2 y2 z2) = sqrt((x2-x1)^ 2+(y2-y1)^2+(z2-z1)^2)

    list2Point [a,b,c] = Point3d a b c