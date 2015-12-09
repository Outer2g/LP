import System.Random

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
			| x <= 0 = "  " ++ tractaFila xs
			| otherwise = show x ++" "++tractaFila xs

buildTauler:: Int -> Int -> TaulerBrid
buildTauler n m = (Taula (aux n m))
	where
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
posValida i j (Taula mat) = i>0 && ((length mat)-1 > i) && j>0 && (length (mat!!i)-1 > j)

{-a partir de un tauler, retorna el numero del jugador que ha guanyat, o -1 si encara
no hi ha guanyador-}
isFinished::TaulerBrid ->Int
isFinished t@(Taula mat)
	| whoWon t 1 = 1
	| whoWon t 2 = 2
	| otherwise = -1
	where
		whoWon::TaulerBrid->Int -> Bool
		whoWon t color
			|color ==1 = checkWin (takeAdjacents 0 (-1) t 1 []) t color
			|otherwise = checkWin (takeAdjacents (-1) 0 t 2 []) t color
		checkWin:: [(Int,Int)] -> TaulerBrid ->Int ->Bool
		checkWin [] t color = False
		checkWin (x:xs) t color
			| color == 1 && isWinnerB (fst x) (snd x) t = True
			| color == 2 && isWinnerR (fst x) (snd x) t = True
			| otherwise = checkWin xs t color
		isWinnerB:: Int->Int ->TaulerBrid ->Bool
		isWinnerB i j t@(Taula mat)
			| i == (length mat) - 1 = True
			| otherwise = False
		isWinnerR::Int->Int->TaulerBrid->Bool
		isWinnerR i j t@(Taula mat)
			| j == (length (mat!!0))-1 = True
			| otherwise = False

takeAdjacents::Int -> Int->TaulerBrid ->Int-> [(Int,Int)]->[(Int,Int)]
takeAdjacents (-1) 0 t@(Taula mat) color [] = makeTheCall t color [] (take (div ((length mat)) 2) $ iterate ((+)2) 1)
	where
		makeTheCall::TaulerBrid -> Int -> [(Int,Int)] -> [Int] -> [(Int,Int)]
		makeTheCall t color visited (x:[]) = takeAdjacents x 0 t color visited
		makeTheCall t color visited (x:xs) = takeAdjacents x 0 t color visited ++ makeTheCall t color visited xs
takeAdjacents 0 (-1) t@(Taula mat) color [] = makeTheCall t color [] magic
	where
		magic = (take (div ((length (mat!!0))) 2) $ iterate ((+)2)1)
		makeTheCall::TaulerBrid -> Int -> [(Int,Int)] -> [Int] -> [(Int,Int)]
		makeTheCall t color visited (x:[]) = takeAdjacents 0 x t color visited
		makeTheCall t color visited (x:xs) = takeAdjacents 0 x t color visited ++ makeTheCall t color visited xs
takeAdjacents i j t@(Taula mat) color visited
	| not (elem upper visited) && getPos (i-1) j t == color = [upper]++takeAdjacents i j t color (visited++
															[upper])++takeAdjacents (i-1) j t color (visited++[upper])
	| not (elem lower visited) && getPos (i+1) j t == color = [lower]++takeAdjacents i j t color (visited++[lower])++
															takeAdjacents (i+1) j t color (visited++[lower])
	| not (elem left visited) && getPos i (j-1) t == color =[left]++takeAdjacents i j t color (visited++
															[left])++takeAdjacents i (j-1) t color (visited++[left])
	| not (elem right visited) && getPos i (j+1) t == color = [right]++takeAdjacents i j t color (visited++[right])++
															takeAdjacents i (j+1) t color (visited++[right])
	| otherwise =[]
	where
		upper = ((i-1),j)
		lower =((i+1),j)
		left = (i,(j-1))
		right = (i,(j+1))

--retorna els posibles moviments del tauler actual
possibles::TaulerBrid ->[(Int,Int)]
possibles t@(Taula mat) = buildList (take (length mat) $ iterate (+1) 0) (take (length (mat!!0)) $ iterate (+1) 0) t
	where
		buildList::[Int]->[Int]->TaulerBrid->[(Int,Int)]
		buildList _ [] t= []
		buildList [] _ t= []
		buildList (x:xs) (y:ys) t
			| getPos x y t == 0 && posValida x y t = [(x,y)]++buildList (x:xs) ys t ++ buildList xs (y:ys) t
			| otherwise = buildList (x:xs) ys t ++ buildList xs (y:ys) t

--tots els gameloops tenen com a parametre el tauler a considerar y el torn del jugador. gameloops = estrategies
gameLoop2 t turn
	| isFinished t /= -1 = do
							putStrLn ("Winner : Player "++show (isFinished t))
							putStrLn $ show t
	| possibles t ==[] = do
						putStrLn ("Empat")
						putStrLn $ show t
	| otherwise = do
				putStrLn ("Player "++show turn++" turn")
				putStrLn "Board:"
				putStrLn $ show t
				putStrLn "select new position"
				x<-getLine
				y<-getLine
				let newT = (makeMove (read x ::Int) (read y::Int) t turn)
				if newT == t
					then gameLoop2 t turn
					else gameLoop2 newT (1+mod turn 2)

gameLoop1 t turn
	| isFinished t /= -1 =do
                          putStrLn ("Winner : Player "++show (isFinished t))
                          putStrLn $ show t
	| possibles t ==[] = do
						putStrLn ("Empat")
						putStrLn $ show t
	| otherwise = do
				putStrLn ("Player "++show turn++" moves")
				std<-newStdGen
				let	newPos = (possibles t)!!(fst (genera std 0 ((length (possibles t))-1)))
				let newT = makeMove (fst newPos) (snd newPos) t turn
				putStrLn ("Moved: "++show newPos)
				putStrLn $show newT
				gameLoop1 newT (1+mod turn 2)
gameLoop3 t turn
	| isFinished t /= -1 =do
                          putStrLn ("Winner : Player "++show (isFinished t))
                          putStrLn $ show t
	| possibles t ==[] = do
						putStrLn ("Empat")
						putStrLn $ show t
	| turn==1 = do
				putStrLn ("Player "++show turn++" moves")
				putStrLn "Board:"
				putStrLn $ show t
				putStrLn "select new position"
				x<-getLine
				y<-getLine
				let newT = (makeMove (read x ::Int) (read y::Int) t turn)
				if newT == t
					then gameLoop3 t turn
					else gameLoop3 newT 2
    | otherwise = do
      				putStrLn ("Player "++show turn++" moves")
      				std<-newStdGen
      				let	newPos = (possibles t)!!(fst (genera std 0 ((length (possibles t))-1)))
      				let newT = makeMove (fst newPos) (snd newPos) t turn
      				putStrLn "Moved: "
      				putStrLn $show newT
      				gameLoop3 newT 1

mode2 = do
	putStrLn "Enter dimensions: "
	files<-getLine
	columns<-getLine
	let tauler = buildTauler (read files :: Int) (read columns ::Int)
	gameLoop2 tauler 1

mode1 = do
	putStrLn "Enter dimensions: "
	files<-getLine
	columns<-getLine
	let tauler = buildTauler (read files :: Int) (read columns ::Int)
	gameLoop1 tauler 1

mode3 = do
	putStrLn "Enter dimensions: "
	files<-getLine
	columns<-getLine
	let tauler = buildTauler (read files :: Int) (read columns ::Int)
	gameLoop3 tauler 1

--generacio d'un nombre aleatori donat una seed (transparencies)
genera :: RandomGen s => s -> Int -> Int -> (Int,s)
genera s lo hi = (x,s1)
	where
		(x,s1) = randomR (lo,hi) s

modeSelector mode
	| mode ==1 = do mode1
	| mode ==2 = do mode2
	| otherwise = do mode3
	--nomes hi ha una estrategia per a CPU: la random, el mode 2 no estaba considerat a l'enunciat
	--pero em vaig equivocar, i ja que estava fet, em semblava bona idea incorporar-ho
main = do
	putStrLn "Select Mode"
	putStrLn "Mode 1: Random CPU vs Random CPU"
	putStrLn "Mode 2: Player vs Player"
	putStrLn "Mode 3: Player vs Random CPU"
	mode<-getLine
	modeSelector (read mode::Int)