module Mydata where

import Useful(joinChar)

type Pos = (Int, Int)

type Na = [String]

data Tree a =  Leaf a | Node (Tree a) (Tree a) | Em  deriving (Eq, Show)

data Mana = Mana T Y 

data T = T Na Ta deriving (Eq, Show)

type Y = T -> T

data Ta = Kaz 
        | Dou
        | Zai Int Sta
        | Con Bool Ctp [Mana]
        | Box Bool Btp (Tree Mana)
        | Wts Pos Pos [Mana] [Mana] [Mana]

instance Eq Ta where
  (==) (Con b1 c1 mns1) (Con b2 c2 mns2) = b1==b2 && c1==c2 && mNames mns1==mNames mns2
  (==) (Box b1 bt1 tm1) (Box b2 bt2 tm2) = b1==b2 && bt1==bt2
                                         && (mNames$flatten tm1)==(mNames$flatten tm2)
  (==) (Wts p11 p12 mns11 mns12 mns13) (Wts p21 p22 mns21 mns22 mns23) =
    p11==p21 && p12==p22 && mNames mns11==mNames mns21 && mNames mns12==mNames mns22 &&
      mNames mns13==mNames mns23
  (==) a b = a==b

instance Eq Mana where
  (==) (Mana t1 _) (Mana t2 _) = t1 == t2

instance Show Ta where
  show (Zai i st) = "--ZAIRYOU*amount:"++(show i)++"*state:"++(show st)
  show (Con b c mns) = "--Container*open:"++(show b)++"*type:"++(show c)
                     ++"*contents:"++(mNames mns)
  show (Box b bt tm) = "--Box*open:"++(show b)++"*type:"++(show bt)
                     ++"*contents:"++(mNames$flatten tm)
  show (Wts p1 p2 mns1 mns2 mns3) = "--Watasi*posFrom:"++(show p1)++"*posTo:"++(show p2)
                     ++"*LHand:"++(mNames mns1)++"*RHand:"++(mNames mns2)++"*manas:"
                     ++(mNames mns3)
  show a          = show a

instance Show Mana where
  show (Mana t _) = show t


data Sta =  Ko  |  Cu  |  Sl  |  Di  |  Li  |  Mi  |  Po  |  Lq   deriving (Eq, Show)
--        kotai,  cut,  slice, dice,  line,  mizin, powder, liquid

data Ctp =  Pa  |  Ho  |  Ba  |  Bo   deriving (Eq, Show)
--        pack,  hotel, ball,  box

data Btp =  Re  |  Fr  |  Sh   deriving (Eq, Show)
--      refridge,freezer,shelf

reiT :: Tree Mana
reiT = Node (Node (Leaf tamanegiP) (Em)) Em 

mName :: Mana -> String
mName (Mana (T na _) _) = head na 

mNames :: [Mana] -> String
mNames mns = joinChar ',' (map mName mns)

getTa :: T -> Ta
getTa (T _ ta) = ta

flatten :: Tree a -> [a]
flatten Em = []
flatten (Leaf x) = [x]
flatten (Node l r) = flatten l ++ flatten r

rep :: Int -> a -> [a]
rep = replicate

(.>) :: Mana -> Mana -> Mana
(.>) (Mana t _) (Mana (T _ Dou) y) = Mana (y t) id
(.>) (Mana (T na ta) _) (Mana (T ma _) _) = Mana (T (na++ma) ta) id

regions :: [(String,Pos)]
regions = [("reizouko",(2,3))]

searchRegion :: String -> Pos
searchRegion = sarReg regions

sarReg :: [(String,Pos)] -> String -> Pos
sarReg [] _ = ((-1),(-1))
sarReg ((nm,pos):rs) str =
  if (nm==str) then pos else sarReg rs str

tamanegi :: Mana
tamanegi = Mana (T ["tamanegi"] (Zai 1 Ko)) id

tamanegiP :: Mana
tamanegiP = Mana (T ["tamanegi"] (Con False Pa (rep 4 tamanegi))) id

reizouko :: Mana
reizouko = Mana (T ["reizouko"] (Box False Re reiT)) id

watasi :: Mana
watasi = Mana (T ["watasi"] (Wts (2,1) (2,1) [] [] [])) id

iku :: Mana
iku = Mana (T ["iku"] Dou) going

going :: Y
going t@(T na (Wts pfr _ l r mns)) =
  let ds = if (length na<2) then "" else last na
      nps = searchRegion ds
   in if (ds=="") then t else T (init na) (Wts pfr nps l r mns)
going a = id a
