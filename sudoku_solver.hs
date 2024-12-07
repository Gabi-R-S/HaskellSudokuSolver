import qualified Data.Array as DA
import qualified Data.Bits as DB
import qualified Data.List as DL
-- import Debug.Trace
type Board= DA.Array (Int,Int) Int
type BoardFillInfo= DA.Array (Int,Int) Int
type Solution = Board


testBoard1::[[Int]]
testBoard1 = [[8,0,7,0,0,4,0,0,9],
              [1,0,9,8,0,2,3,0,7],
              [0,3,5,0,0,7,4,0,6],

              [6,0,4,7,8,0,9,3,0],
              [9,0,3,0,0,1,0,7,0],
              [7,8,0,0,0,3,1,4,0],
              
              [0,7,1,4,0,9,8,0,3],
              [4,2,0,3,7,6,0,9,1],
              [3,9,0,1,0,8,7,0,4]]

numsFromBitMask:: Int->[Int]
numsFromBitMask mask= [i| i<-[0..31], mask DB..&. (DB.bit i) /= 0] 


snd'::(a,b,c)->b
snd'(_,b,_)=b



fillNext::BoardFillInfo-> [BoardFillInfo]
fillNext bfi= foldl (\acc x->
    (
        bfi
        DA.// (
        [ ((i,nextPosY), DB.clearBit num x )|i<-[0..8], let num=bfi DA.! (i,nextPosY)]
        ++
        [ ((nextPosX,j), DB.clearBit num x )|j<-[0..8], let num=bfi DA.! (nextPosX,j)]
        ++
        [ ((i,j), DB.clearBit num x )|i<-[bigSquareX..(bigSquareX +2)],j<-[bigSquareY..(bigSquareY +2)], let num=bfi DA.! (i,j)]
        ))
    :acc) [] availableNums   
    where sortedVals= (DL.sortOn snd' 
            [((x,y),length bits, bits)|
            x<-[0..8],y<-[0..8],
            let bits=numsFromBitMask (bfi DA.! (x,y)), (length bits)>1])
          ((nextPosX,nextPosY),_,availableNums)= if sortedVals==[] then undefined else (head sortedVals)
          bigSquareX= (div nextPosX 3)*3
          bigSquareY= (div nextPosY 3)*3

boardFillInfoToBoard::BoardFillInfo-> Board
boardFillInfoToBoard bfi= DA.array ((0,0),(8,8)) [((x,y), if length bits==1 then (head bits)+1 else (if length bits == 0 then -1 else 0)) |((x,y),n)<-DA.assocs bfi, let bits = numsFromBitMask n]


boardToBoardFillInfo::Board-> BoardFillInfo
boardToBoardFillInfo board= foldl (\acc (x,y) ->
    let bigSquareX= (div x 3)*3 
        bigSquareY= (div y 3) *3
        v = (acc DA.! (x,y))
        msk=numsFromBitMask v
        val = if length msk == 0 then 0 else (head (msk)) in
            if length msk ==1 then
      acc DA.// (
        [ ((i,y), DB.clearBit  num val )|i<-[0..8], i/=x, let num=acc DA.! (i,y)]
        ++
        [((x,j), DB.clearBit num val )|j<-[0..8], j/=y, let num=acc DA.! (x,j)]
        ++
        [ ( (i,j), DB.clearBit  num val )|i<-[bigSquareX..(bigSquareX +2)],j<-[bigSquareY..(bigSquareY +2)],j/=y|| i/=x, let num=acc DA.! (i,j)]
        ) else acc
    ) (start ) [(x,y)|x<-[0..8],y<-[0..8]]   
    where start= DA.array ((0,0), (8,8)) [((x,y), if n==0 then foldl (\a v->a DB..|. (DB.bit v)) 0 [0..8] else DB.bit (n-1) ) |((x,y),n)<-DA.assocs board]


hasEmptySpace::BoardFillInfo->Bool
hasEmptySpace bfi= any (\x->length (numsFromBitMask x) > 1) ((DA.elems bfi))

fillIteration::BoardFillInfo ->BoardFillInfo
fillIteration bfi = foldl (\acc (x,y) ->
    let bigSquareX= (div x 3)*3 
        bigSquareY= (div y 3) *3
        v= (acc DA.! (x,y)) 
        msk=numsFromBitMask v
        val = if length msk == 0 then 0 else (head (msk))
        in
            if length msk ==1 then
     acc DA.// (
        [ ((i,y),  DB.clearBit num val )|i<-[0..8],x/=i, let num=acc DA.! (i,y)]
        ++
        [ ((x,j), DB.clearBit num val)|j<-[0..8],y/=j, let num=acc DA.! (x,j)]
        ++
        [ ((i,j), DB.clearBit num val )|i<-[bigSquareX..(bigSquareX +2)],j<-[bigSquareY..(bigSquareY +2)],x/=i|| y/=j, let num=acc DA.! (i,j)]
        )else acc
    ) bfi [(x,y)|x<-[0..8],y<-[0..8]] 


solveRec::BoardFillInfo->[BoardFillInfo]
solveRec bfi= if hasEmptySpace (bfi) then
        ( if bfi==it then 
            (foldl (\acc x ->
                acc++(
                    if hasEmptySpace x then     
                       acc++( foldl (\a i->i:a) [] (solveRec x))
                    else 
                        ( x):acc)) 
                [] (fillNext bfi)) 
        else (solveRec it))
    else  [bfi]
    where it = fillIteration bfi   

solve:: Board -> [Solution]
solve board= if hasEmptySpace fillInfo then     
    map boardFillInfoToBoard (solveRec fillInfo)
    else [board]
    where fillInfo = (boardToBoardFillInfo board)



printSolutions::[Solution]->IO()
printSolutions (s:ss) = do
    let v=foldl (flip (++)) [] [ foldl ( (++)) [] [if x==9 then "\n" else ((show(s DA.!(x,y))))|x<-[0..9] ] |y<-[0..8]]

    putStrLn "Solution:"
    putStrLn v
    printSolutions ss

printSolutions [] = return ( )
    




listToBoard::[[Int]]->Board
listToBoard l = DA.array ((0,0),(8,8)) [((x,y), max 0 (min 9 (l !!y!!x)))|x<-[0..8],y<-[0..8]]


