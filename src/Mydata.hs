module Mydata where

import Useful(joinChar)

data Mana = Mana T Y | Em

type Y = T -> T

data T = S String Ta | N Int deriving (Eq, Show)

data Ta = Zai Int Sta
        | Con Bool Ctp [Mana]
        | All [Mana]

instance Eq Ta where
  (==) (Con b1 c1 mns1) (Con b2 c2 mns2) = b1==b2 && c1==c2 && (mNames mns1 == mNames mns2)
  (==) (All mns1) (All mns2) = mNames mns1 == mNames mns2 
  (==) a b = a==b

instance Eq Mana where
  (==) Em Em                   = True
  (==) (Mana t1 _) (Mana t2 _) = t1 == t2
  (==) _ _                     = False

instance Show Ta where
  show (Zai i st) = "--ZAIRYOU*amount:"++(show i)++"*state:"++(show st)
  show (Con b c mns) = "--Container*open:"++(show b)++"*type:"++(show c)
                     ++"*contents:"++(mNames mns)
  show (All mns) = mNames mns

instance Show Mana where
  show Em = "empty"
  show (Mana t _) = show t


data Sta =  Ko  |  Cu  |  Sl  |  Di  |  Li  |  Mi  |  Po  |  Lq   deriving (Eq, Show)
--        kotai,  cut,  slice, dice,  line,  mizin, powder, liquid

data Ctp =  Pa  |  Ho  |  Ba  |  Bo   deriving (Eq, Show)
--        pack,  hotel, ball,  box

mName :: Mana -> String
mName (Mana (S s _) _) = s
mName (Mana (N i) _) = show i
mName _ = "empty"

mNames :: [Mana] -> String
mNames mns = joinChar ',' (map mName mns)

rep :: Int -> a -> [a]
rep = replicate

tamanegi :: Mana
tamanegi = Mana (S "tamanegi" (Zai 1 Ko)) id

tamanegiP :: Mana
tamanegiP = Mana (S "tamanegi" (Con False Pa (rep 4 tamanegi))) id
