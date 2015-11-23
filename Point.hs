import Data.List
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
    --Donat un numero binari, retorna el seu decimal
    bin2dec:: [Int] -> Double
    bin2dec xs = sum (map (2^) ( findIndices (==1) xs))



    dist (Point3d x1 y1 z1) (Point3d x2 y2 z2) = sqrt((x2-x1)^ 2+(y2-y1)^2+(z2-z1)^2)

    list2Point [a,b,c] = Point3d a b c