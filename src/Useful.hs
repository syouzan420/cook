module Useful(getIndex,joinChar,sepChar,replCon,sepEL,iniEL) where

--import Data.Char(isDigit)

getIndex :: Eq a => a -> [a] -> Int
getIndex _ [] = 0
getIndex t (x:xs) = if(t==x) then 0 else 1+(getIndex t xs)

sepChar :: Char -> String -> [String]
sepChar _ [] = []
sepChar ch (x:xs)  
  |xs==[] = if (x==ch) then [[]] else [[x]]
  |otherwise = let sch = sepChar ch xs
                   (hd,tl) = (head sch,tail sch) 
                in if (x==ch) then [[]]++(hd:tl)
                              else [x:hd]++tl

--sepChar :: Char -> String -> [String]
--sepChar _ [] = []
--sepChar ch (x:[])    = if (x==ch) then [[]] else [[x]]
--sepChar ch (x:xs) = if (x==ch) then [[]]++(hd:tl)
--                               else [x:hd]++tl
--                          where (hd:tl) = sepChar ch xs

joinChar :: Char -> [String] -> String
joinChar _ [] = []
joinChar _ [x] = x
joinChar ch (x:xs) = x++[ch]++(joinChar ch xs)

--dsort :: Ord a => [(a,b)] -> [b] 
--dsort dt = snd$unzip$sorting dt

--sorting :: Ord a => [(a,b)] -> [(a,b)]
--sorting [] = []
--sorting ((a,b):xs) = sorting sml ++ [(a,b)] ++ sorting lar
--   where sml = [(p,n) | (p,n) <- xs, p<a]
--         lar = [(q,m) | (q,m) <- xs, q>=a]

--toList :: (Enum a,Ord a) => a -> a -> [a]
--toList a b = if(a==b) then [a] else
--             if (a<b) then [a..b] else [b..a]

--isNum :: String -> Bool
--isNum [] = True
--isNum (x:xs) = (isDigit x) && (isNum xs)

--isChar :: String -> String -> Bool
--isChar [] _ = True 
--isChar (x:xs) str = (elem x str) && (isChar xs str)

--isStr :: String -> Bool
--isStr [] = True
--isStr (x:xs) = (not$isDigit x) && (isStr xs)

--chooseData :: String -> [String] -> [String]
--chooseData _ [] = []
--chooseData h (x:xs) =
--  let lh = length h
--   in if ((take lh x)==h) then (drop lh x):(chooseData h xs) else chooseData h xs

replCon :: Int -> a -> [a] -> [a]
replCon i x y = take i y ++ [x] ++ drop (i+1) y 

--delCon :: Int -> [a] -> [a]
--delCon i y = take i y ++ drop (i+1) y 

--dataSub :: Eq a => [a] -> [a] -> [a]
--dataSub org [] = org 
--dataSub org (t:ts) =
--  let ie = elem t org
--      i = if ie then getIndex t org else (-1)
--   in if (i>(-1)) then dataSub (delCon i org) ts
--                   else dataSub org t

--dataAdd :: Eq a => [a] -> [a] -> [a]
--dataAdd org [] = org 
--dataAdd org (t:ts) =
--  let ie = elem t org
--   in if ie then dataAdd org ts
--            else dataAdd (org++[t]) ts

sepEL :: Eq a => a -> [a] -> [a]
sepEL _ [] = []
sepEL e (x:xs) = if (e==x) then xs else sepEL e xs 

iniEL :: Eq a => a -> [a] -> [a]
iniEL _ [] = []
iniEL e (x:xs) = if (e==x) then [x] else x:iniEL e xs
