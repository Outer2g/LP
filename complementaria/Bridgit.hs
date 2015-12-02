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

{-a partir de un tauler, retorna el numero del jugador que ha guanyat, o -1 si encara
no hi ha guanyador-}
isFinished::TaulerBrid ->Int
isFinished t@(Taula mat)
	| blueWon t = 1
	| redWon t = 2
	| otherwise = -1
	where
	blueWon::TaulerBrid -> Bool
	blueWon t = checkWinB (takeAdjacents 0 1 t 1) t
	checkWinB::[(Int,Int)]-> TaulerBrid -> Bool
	checkWinB [] t = False
	checkWinB (x:xs) t
		| isWinnerB (fst x) (snd x) t = True
		| otherwise = checkWinB (xs++takeAdjacents (fst x) (snd x) t 1) t
	isWinnerB:: Int->Int-> TaulerBrid -> Bool
	isWinnerB i j t@(Taula mat)
		| i == (length mat)-1 && getPos i j t == 1 = True
		| otherwise = False
	redWon::TaulerBrid ->Bool
	redWon t = checkWinR (takeAdjacents 1 0 t 1) t
	checkWinR::[(Int,Int)]-> TaulerBrid -> Bool
	checkWinR [] t = False
	checkWinR (x:xs) t
		| isWinnerR (fst x) (snd x) t = True
		| otherwise = checkWinR (xs++takeAdjacents (fst x) (snd x) t 2) t
	isWinnerR:: Int->Int-> TaulerBrid -> Bool
	isWinnerR i j t
		| posValida i j t && j == (length (mat!!i))-1 && getPos i j t ==2 = True
		|otherwise = False


takeAdjacents::Int -> Int->TaulerBrid ->Int->[(Int,Int)]
takeAdjacents i j t@(Taula mat) color
	| posValida (i-1) j t && getPos (i-1) j t == color = [((i-1),j)] ++ takeAdjacents i j t color
	| posValida (i+1) j t && getPos (i+1) j t == color = [((i+1),j)] ++ takeAdjacents i j t color
	| posValida i (j-1) t && getPos i (j-1) t == color = [(i,(j-1))] ++ takeAdjacents i j t color
	| posValida i (j+1) t && getPos i (j+1) t == color = [(i,(j+1))] ++ takeAdjacents i j t color
	| otherwise = []


main = do 
	files<-getLine
	columns<-getLine
	let tauler = buildTauler (read files :: Int) (read columns ::Int)
	print tauler