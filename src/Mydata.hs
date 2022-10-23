module Mydata where

import Useful(joinChar)

data Mana = Mana T Y | Em

type Y = T -> T

data T = S String Ta | N Int deriving (Eq, Show)

data Ta = Zai Int Sta
        | All [Mana]

instance Eq Mana where
  (==) Em Em                   = True
  (==) (Mana t1 _) (Mana t2 _) = t1 == t2
  (==) _ _                     = False

instance Eq Ta where
  (==) (All mns1) (All mns2) = mNames mns1 == mNames mns2 
  (==) a b = a==b

instance Show Ta where
  show (Zai i st) = "--ZAIRYOU-- amount:"++(show i)++" state:"++(show st)
  show (All mns) = mNames mns

data Sta =  Ko  |  Cu  |  Sl  |  Di  |  Li  |  Mi  |  Po  |  Lq   deriving (Eq, Show)
--        kotai,  cut,  slice, dice,  line,  mizin, powder, liquid
--

mName :: Mana -> String
mName (Mana (S s _) _) = s
mName (Mana (N i) _) = show i
mName _ = "empty"

mNames :: [Mana] -> String
mNames mns = joinChar ',' (map mName mns)
