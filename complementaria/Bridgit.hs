data TaulerBrid = Taula [[Int]]

{-buildTauler:: Int -> Int -> TaulerBrid
buildTauler nFiles nColumnes = (Taula [filaB,-}

aux::Int->[[Int]]
aux n = (concat $ replicate n ([take (n+n-1) $ cycle [0,1]] ++ [take (n+n-1) $ cycle [2,0]]))
	++ [take (n+n-1) $ cycle [0,1]]
  
main = do 
	files<-getLine
	let tauler = aux (read files :: Int)
	print tauler