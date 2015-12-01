data TaulerBrid = Taula [[Int]] deriving(Eq)

instance Show TaulerBrid where
	show t = imprim t
		where
		imprim::TaulerBrid -> String
		imprim (Taula []) = ""
		imprim (Taula (x:xs)) = tractaFila x++"\n"++imprim (Taula xs)
		tractaFila::[Int] ->String
		tractaFila ([]) = ""
		tractaFila (x:xs)
			| x == 0 = "  " ++ tractaFila xs
			| otherwise = show x ++" "++tractaFila xs

buildTauler:: Int -> Int -> TaulerBrid
buildTauler n m = (Taula (aux n m))

aux::Int->Int->[[Int]]
aux n m = (concat $ replicate m ([take (n+m) $ cycle [0,1]] ++ [take (n+m) $ cycle [2,0]]))
	++ [take (n+m) $ cycle [0,1]]

makeMove::Int -> Int -> TaulerBrid -> Int-> TaulerBrid
makeMove i j t color
	| color /= colorCasella && colorCasella /=0 = t
	| otherwise = setPos i j t color
		where
		colorCasella = getPos i j t


setPos:: Int -> Int -> TaulerBrid-> Int -> TaulerBrid
setPos i j t@(Taula mat) color
	| posValida i j t = (Taula insertaho)
	| otherwise = t
	where
	insertaho = fst spliti ++ [novaFila] ++ drop 1 (snd spliti)
	spliti = splitAt i mat
	novaFila = fst splitj ++ [color] ++ drop 1 (snd splitj)
	splitj = splitAt j (mat!!i)

getPos:: Int -> Int -> TaulerBrid -> Int
getPos i j t@(Taula mat)
 	| posValida i j t = (mat!!i)!!j
 	| otherwise = -1

posValida::Int -> Int -> TaulerBrid -> Bool
posValida i j (Taula mat) = ((length mat) > i) && (length (mat!!i) > j)


main = do 
	files<-getLine
	columns<-getLine
	let tauler = buildTauler (read files :: Int) (read columns ::Int)
	print tauler