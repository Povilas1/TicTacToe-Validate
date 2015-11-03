module Validate 
where

import Data.List
import Data.List.Utils
import Data.List.Split
--------------(   V  ,    X  ,    Y  )
type Action = (String, String, String)
type Cords = (String, String)
type Board = [Action]

--validBoards
msgV1 = "ld1:v1:x1:xi1e1:yi2eed1:v1:o1:xi1e1:yi1eed1:v1:x1:xi2e1:yi0eed1:v1:o1:xi2e1:yi2eed1:v1:x1:xi0e1:yi0eee"
msgV2 = "ld1:v1:x1:xi1e1:yi2eed1:v1:o1:xi1e1:yi0eed1:v1:x1:xi0e1:yi0eed1:v1:o1:xi2e1:yi0eed1:v1:x1:xi0e1:yi2eee"
msgV3 = "ld1:v1:x1:xi2e1:yi0eed1:v1:o1:xi1e1:yi0eed1:v1:x1:xi1e1:yi2eed1:v1:o1:xi1e1:yi1eed1:v1:x1:xi0e1:yi2eed1:v1:o1:xi2e1:yi2eed1:v1:x1:xi0e1:yi1eee"
msgV4 = "ld1:v1:x1:xi1e1:yi2eed1:v1:o1:xi1e1:yi1eed1:v1:x1:xi2e1:yi0eed1:v1:o1:xi0e1:yi0eed1:v1:x1:xi1e1:yi0eed1:v1:o1:xi2e1:yi2eed1:v1:x1:xi0e1:yi1eed1:v1:o1:xi2e1:yi1eed1:v1:x1:xi0e1:yi2eee"
--collidedValues
msgC = "ld1:v1:x1:xi2e1:yi0eed1:v1:o1:xi0e1:yi0eed1:v1:x1:xi1e1:yi1eed1:v1:o1:xi2e1:yi2eed1:v1:x1:xi1e1:yi0eed1:v1:x1:xi2e1:yi2eee"

--TooMuchMoves
msgM1 = "ld1:v1:x1:xi1e1:yi0eed1:v1:o1:xi2e1:yi0eed1:v1:x1:xi2e1:yi2eed1:v1:o1:xi0e1:yi2eed1:v1:x1:xi0e1:yi0eed1:v1:x1:xi1e1:yi1eee"
msgM2 = "ld1:v1:x1:xi2e1:yi1eed1:v1:o1:xi0e1:yi1eed1:v1:x1:xi1e1:yi0eed1:v1:o1:xi2e1:yi0eed1:v1:x1:xi1e1:yi2eed1:v1:x1:xi2e1:yi2eee"

decode :: String -> Board
decode ('l' : msg) = 
    let
    (external, _) = decodeBoard msg []
    in external
decode _ = error "No message"

decodeBoard :: String -> Board -> (Board, String)
decodeBoard ('e':left) acc = (acc, left)
decodeBoard ('d':dict) acc =
    let
    (d,left) = readAction dict
    in decodeBoard left (d:acc)
decodeBoard str ext = error ("Invalid message. Unparsed content: " ++ str) 



readAction :: String -> (Action, String)
readAction str =
    let
        (key1, val1, left) = readItem str
        (key2, val2, left2) = readItem left
        (key3, val3, left3) = readItem left2
        ('e' : left4) = left3
        items = [(key1, val1),(key2, val2),(key3, val3)]
        v1 = match (=='v') items
        v2 = match (=='x') items
        v3 = match (=='y') items
    in ((v1,v2,v3), left4)

readCords :: String -> (Cords, String)
readCords str =
    let
        (key1, val1, left) = readItem str
        (key2, val2, left2) = readItem left
        (key3, val3, left3) = readItem left2
        ('e' : left4) = left3
        items = [(key1, val1),(key2, val2),(key3, val3)]
        v1 = match (=='v') items
        v2 = match (=='x') items
        v3 = match (=='y') items
    in ((v2,v3), left4)


readItem :: String -> (String, String, String)
readItem str =
    let
        (key, left) = readSymbol str
        (value, left') = readSymbol left
    in (key, value, left')

readSymbol :: String -> (String, String)
readSymbol ('1' : ':' : rest) = readLetter rest
readSymbol ('i' : rest) = readNumber rest
readSymbol str = error ("Invalid message. Unparsed content: " ++ str)

readLetter :: String -> (String, String)
readLetter ( a : rest) = ([a], rest)

readNumber :: String -> (String, String)
readNumber str = 
    let
        num = takeWhile (/='e') str
        left = drop (length num + 1) str
    in (num, left)


match :: (Char -> Bool) -> [(String, String)] -> String
match mch [] = ""
match mch (([],v) : rest) = match mch rest
match mch ( ((b:_),v) : rest) = 
    if 
        mch b
    then 
        v 
    else (match mch rest)


sortByX (_, b1, _) (_, b2, _) = compare b1 b2
sortByY (_, _, c1) (_, _, c2) = compare c1 c2

winner :: Board -> String
winner mp = if (length arrays' > 0) then (arrays') else ("n")
    where

        xArrays = groupBy (\x y->(value x "x") == (value y "x")) (sortBy sortByX mp)
        yArrays = groupBy (\x y->(value x "y") == (value y "y")) (sortBy sortByY mp)

        diagon1Array = mp >>= (\e -> if ((value e "x") == (value e "y")) then [e] else [])
        diagon2Array = mp >>= (\e -> if ((read (value e "x") :: Int) == (3-(read (value e "y") :: Int))) then [e] else [])

        allArrays = merge xArrays $ merge yArrays [diagon1Array, diagon2Array]
        arrays' = allArrays >>= (\e -> if (length e == 3 && isWinner e) then (value (e !! 1) "v") else [])


value :: Action -> String -> String
value (v0,_,_) "v" = v0
value (_,v1,_) "x" = v1
value (_,_,v2) "y" = v2


isWinner :: Board -> Bool
isWinner mp = if (length grouped == 1) then True else False
    where
        grouped = groupBy (\x y->(value x "v") == (value y "v")) mp

isValidWinner :: Board -> Bool
isValidWinner brd = if (length (winner brd)) > 1 then False else True 


isBoardValid :: Board -> Bool
isBoardValid brd = (areTwoInARowMoves brd) && (isValidWinner brd) && (areCollision brd)


sortGT (a1, b1, c1) (a2, b2, c2)
  | b1 < b2 = GT
  | b1 > b2 = LT
  | b1 == b2 = compare c1 c2

areCollision :: Board -> Bool
areCollision mp = if (length arrayCollided' > 0) then False else True 
    where
        sorted = sortBy sortGT mp
        collidedV = groupBy (\a b->((value a "x") == (value b "x")) && ((value a "y") == (value b "y"))) sorted
        arrayCollided' = collidedV >>= (\e -> if (length e == 2) then ("c") else [])

parsedValues :: Board -> [Char]
parsedValues brd = concatMap (\e -> value e "v") brd

areTwoInARowMoves :: Board -> Bool
areTwoInARowMoves brd = if (isInfixOf "xx" (parsedValues brd)) || (isInfixOf "oo" (parsedValues brd))
    then False
    else True

checkBoard :: String -> Bool
checkBoard message = isBoardValid (decode message)