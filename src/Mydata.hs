module Mydata where

import Useful(joinChar)

type Pos = (Int, Int)

data Tree a =  Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show)

data Mana = Mana T Y | Em

type Y = T -> T

data T = S String Ta | N Int deriving (Eq, Show)

data Ta = Zai Int Sta
        | Con Bool Ctp [Mana]
        | Box Bool Btp (Tree Mana)
        | Wts Pos Pos [Mana] [Mana] [Mana] [String]
        | All [Mana]

instance Eq Ta where
  (==) (Con b1 c1 mns1) (Con b2 c2 mns2) = b1==b2 && c1==c2 && mNames mns1==mNames mns2
  (==) (Box b1 bt1 tm1) (Box b2 bt2 tm2) = b1==b2 && bt1==bt2
                                         && (mNames$flatten tm1)==(mNames$flatten tm2)
  (==) (Wts p11 p12 mns11 mns12 mns13 ord1) (Wts p21 p22 mns21 mns22 mns23 ord2) =
    p11==p21 && p12==p22 && mNames mns11==mNames mns21 && mNames mns12==mNames mns22 &&
      mNames mns13==mNames mns23 && ord1==ord2
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
  show (Box b bt tm) = "--Box*open:"++(show b)++"*type:"++(show bt)
                     ++"*contents:"++(mNames$flatten tm)
  show (Wts p1 p2 mns1 mns2 mns3 ord) = "--Watasi*posFrom:"++(show p1)++"*posTo:"++(show p2)
                     ++"*LHand:"++(mNames mns1)++"*RHand:"++(mNames mns2)++"*manas:"
                     ++(mNames mns3)++"*orders:"++(show ord)
  show (All mns) = mNames mns

instance Show Mana where
  show Em = "empty"
  show (Mana t _) = show t


data Sta =  Ko  |  Cu  |  Sl  |  Di  |  Li  |  Mi  |  Po  |  Lq   deriving (Eq, Show)
--        kotai,  cut,  slice, dice,  line,  mizin, powder, liquid

data Ctp =  Pa  |  Ho  |  Ba  |  Bo   deriving (Eq, Show)
--        pack,  hotel, ball,  box

data Btp =  Re  |  Fr  |  Sh   deriving (Eq, Show)
--      refridge,freezer,shelf

mName :: Mana -> String
mName (Mana (S s _) _) = s
mName (Mana (N i) _) = show i
mName _ = "empty"

mNames :: [Mana] -> String
mNames mns = joinChar ',' (map mName mns)

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l r) = flatten l ++ flatten r

rep :: Int -> a -> [a]
rep = replicate

tamanegi :: Mana
tamanegi = Mana (S "tamanegi" (Zai 1 Ko)) id

tamanegiP :: Mana
tamanegiP = Mana (S "tamanegi" (Con False Pa (rep 4 tamanegi))) id
