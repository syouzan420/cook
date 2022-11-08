module Mydata where

import Useful(joinChar,replCon,getIndex)

type Pos = (Int, Int)

type Na = [String]

data Tr a = Lf a | Nd [Tr a] deriving (Eq, Show)

data Mana = Mana T Y

data T = T Na Ta deriving (Eq, Show)

type Y = T -> T -> T

data Ta = Kaz 
        | Moz
        | Dou
        | Zai Int Sta
        | Con Bool Ctp [Mana]
        | Box Bool Btp [Mana]
        | Wts Pos Pos [Mana] [Mana] [Mana]

instance Eq Ta where
  (==) (Con b1 c1 mns1) (Con b2 c2 mns2) = b1==b2 && c1==c2 && mNames mns1==mNames mns2
  (==) (Box b1 bt1 mns1) (Box b2 bt2 mns2) = b1==b2 && bt1==bt2 && mNames mns1==mNames mns2
  (==) (Wts p11 p12 mns11 mns12 mns13) (Wts p21 p22 mns21 mns22 mns23) =
    p11==p21 && p12==p22 && mNames mns11==mNames mns21 && mNames mns12==mNames mns22 &&
      mNames mns13==mNames mns23
  (==) a b = a==b

instance Eq Mana where
  (==) (Mana t1 _) (Mana t2 _) = t1 == t2

instance Show Ta where
  show (Zai i st) = "--ZAI*am:"++(show i)++"*st:"++(show st)
  show (Con b c mns) = "--CON*op:"++(show b)++"*tp:"++(show c)++"*con:"++(mNames mns)
  show (Box b bt mns) = "--BOX*op:"++(show b)++"*tp:"++(show bt)++"*con:"++(mNames mns)
  show (Wts p1 p2 mns1 mns2 mns3) = "--WTS*posFrom:"++(show p1)++"*posTo:"++(show p2)
                     ++"*LH:"++(mNames mns1)++"*RH:"++(mNames mns2)++"*manas:"
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

data Dir =  Lt  |  Rt  |  Up  |  Dn   deriving (Eq, Show)

reiT :: Tr Mana
reiT = Nd [Nd [Nd []
              ,Nd [Lf tamanegiP]]
          ,Nd [Lf tamanegiP]] 

reiL :: [Mana]
reiL = flatten reiT

flatten :: Tr a -> [a]
flatten (Nd []) = []
flatten (Nd (x:xs)) = flatten x ++ flatten (Nd xs)
flatten (Lf a) = [a]

mName :: Mana -> String
mName (Mana (T na _) _) = head na 

mNames :: [Mana] -> String
mNames mns = joinChar ',' (map mName mns)

rep :: Int -> a -> [a]
rep = replicate

(.>) :: Maybe Mana -> Maybe Mana -> Maybe Mana
(.>) (Just (Mana t1 _)) (Just (Mana t2 y)) = Just (Mana (y t1 t2) doNothing)
(.>) _ _ = Nothing

getT :: Mana -> T
getT (Mana t _) = t

getY :: Mana -> Y
getY (Mana _ y) = y

getTY :: Mana -> (T,Y)
getTY (Mana t y) = (t,y)

regions :: [(String,Pos)]
regions = [("reizouko",(2,3))]

searchRegion :: String -> Pos
searchRegion = sarReg regions

sarReg :: [(String,Pos)] -> String -> Pos
sarReg [] _ = ((-1),(-1))
sarReg ((nm,pos):rs) str =
  if (nm==str) then pos else sarReg rs str

manas :: [(String,Mana)]
manas = [("tamanegi",tamanegi),("tamanegiP",tamanegiP),("reizouko",reizouko)
        ,("watasi",watasi),("iku",iku),("akeru",akeru)]

toMana :: String -> Maybe Mana
toMana str =
  let (s,m) = unzip manas
      i = if (elem str s) then getIndex str s else (-1)
   in if (i==(-1)) then Nothing else Just (m!!i)

tamanegi :: Mana
tamanegi = Mana (T ["tamanegi"] (Zai 1 Ko)) addOrd 

tamanegiP :: Mana
tamanegiP = Mana (T ["tamanegi"] (Con False Pa (rep 4 tamanegi))) addOrd 

reizouko :: Mana
reizouko = Mana (T ["reizouko"] (Box False Re reiL)) addOrd 

watasi :: Mana
watasi = Mana (T ["watasi"] (Wts (2,1) (2,1) [] [] [reizouko])) doNothing 

iku :: Mana
iku = Mana (T ["iku"] Dou) going

akeru :: Mana
akeru = Mana (T ["akeru"] Dou) openY

doNothing :: Y
doNothing t _ = t

going :: Y
going t@(T na (Wts pfr _ l r mns)) _ =
  let ds = if (length na<2) then "" else last na
      nps = searchRegion ds
   in if (ds=="") then t else T (init na) (Wts pfr nps l r mns)
going t1 _ = t1 

addOrd :: Y
addOrd (T na1 ta) (T na2 _) = T (na1++na2) ta

openY :: Y
openY t@(T na@(_:ord) (Wts pfr pto l r mns)) _ =
  let ds = if (ord==[]) then "" else last ord
      nps = searchRegion ds
      iex = pfr == pto && pto == nps
      tmi = if (ds=="" || iex==False) then (-1) else searchMana ds mns
      nmn = if (tmi==(-1) || tmi==length ds) then mns 
                                             else let (tt,ty) = getTY (mns!!tmi)
                                                   in replCon tmi (Mana (openT tt) ty) mns
   in if (tmi==(-1)) then t else (T na (Wts pfr pto l r nmn))
openY t _ = t

searchMana :: String -> [Mana] -> Int 
searchMana _ [] = 0 
searchMana ds ((Mana (T na _) _):ms) = if (ds==head na) then 0 else 1+searchMana ds ms

openT :: T -> T
openT (T na (Con _ c mns)) = T na (Con True c mns)
openT (T na (Box _ bt mns)) = T na (Box True bt mns)
openT t = t
