module Mydata(Pos,Rch,Mana(..),T(..),Ta(Wts),(.>),manas,watasi,toMana,posToRch
             ,reiT,flatten,getFStr) where

import Useful(joinChar,replCon,getIndex,sepEL,iniEL)

type Pos = (Int, Int)

type Rch = [String] -- reachable

type Na = [String]

data Tr a = Nd a [Tr a] deriving (Eq, Show)

data Mana = Mana T Y

data T = T Na Ta deriving (Eq, Show)

type Y = T -> T -> T

data Ta = Dou
        | Pla
        | Zai Int Sta
        | Con Bool Ctp [Mana]
        | Box Bool Btp (Tr Mana)
        | Wts Pos Pos [Mana] [Mana] [Mana] Rch

instance Eq Ta where
  (==) (Con b1 c1 mns1) (Con b2 c2 mns2) = b1==b2 && c1==c2 && mNames mns1==mNames mns2
  (==) (Box b1 bt1 mnt1) (Box b2 bt2 mnt2) = b1==b2 && bt1==bt2 && mnt1==mnt2
  (==) (Wts p11 p12 mns11 mns12 mns13 rc1) (Wts p21 p22 mns21 mns22 mns23 rc2) =
    p11==p21 && p12==p22 && mNames mns11==mNames mns21 && mNames mns12==mNames mns22 &&
      mNames mns13==mNames mns23 && rc1==rc2
  (==) a b = a==b

instance Eq Mana where
  (==) (Mana t1 _) (Mana t2 _) = t1 == t2

instance Show Ta where
  show (Zai i st) = "--ZAI*am:"++(show i)++"*st:"++(show st)
  show (Con b c mns) = "--CON*op:"++(show b)++"*tp:"++(show c)++"*con:"++(mNames mns)
  show (Box b bt mnt) = "--BOX*op:"++(show b)++"*tp:"++(show bt)++"*con:"++(show$mNames$flatten mnt)
  show (Wts p1 p2 mns1 mns2 mns3 rc) = "--WTS*posFrom:"++(show p1)++"*posTo:"++(show p2)
                     ++"*LH:"++(mNames mns1)++"*RH:"++(mNames mns2)++"*mns:"
                     ++(mNames mns3)++"*rch:"++(joinChar ',' rc)
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
reiT = Nd miki [Nd ue [] 
               ,Nd sita [Nd hidari [Nd tamanegiP []]
                        ,Nd migi []]]

getFStr :: [String] -> Tr Mana -> [String]
getFStr [] (Nd _ ts) = map (\(Nd mn _) -> mName mn) ts
getFStr (str:xs) t@(Nd _ as)
    |str=="ue" || str=="hidari" = getFStr xs (as!!0)
    |str=="sita" || str=="migi" = getFStr xs (as!!1)
    |otherwise = getFStr xs t

getRch :: T -> [String]
getRch (T _ (Con _ _ mns)) = map mName mns
getRch (T _ (Box _ _ mnt)) = getFStr [] mnt
getRch _ = []

flatten :: Tr a -> [a]
flatten (Nd _ []) = []
flatten (Nd _ (x:y:[])) = flatten x ++ flatten y 
flatten (Nd _ b) = map (\(Nd mn _) -> mn) b 

mName :: Mana -> String
mName (Mana (T na _) _) = head na 

mNames :: [Mana] -> String
mNames mns = joinChar ',' (map mName mns)

rep :: Int -> a -> [a]
rep = replicate

(.>) :: Maybe Mana -> Maybe Mana -> Maybe Mana
(.>) (Just (Mana t1 _)) (Just (Mana t2 y)) = Just (Mana (y t1 t2) doNothing)
(.>) _ _ = Nothing

getTY :: Mana -> (T,Y)
getTY (Mana t y) = (t,y)

regions :: [(String,Pos)]
regions = [("reizouko",(2,3))]

posToRch :: Pos -> Rch
posToRch ps = sea ps regions
  where sea _ [] = []
        sea ps' ((r,p):xs) = if (ps'==p) then r:(sea ps' xs) else sea ps' xs

searchRegion :: String -> Pos
searchRegion = sarReg regions

sarReg :: [(String,Pos)] -> String -> Pos
sarReg [] _ = ((-1),(-1))
sarReg ((nm,pos):rs) str =
  if (nm==str) then pos else sarReg rs str

manas :: [(String,Mana)]
manas = [("tamanegi",tamanegi),("tamanegiP",tamanegiP),("reizouko",reizouko)
        ,("watasi",watasi),("miki",miki),("hidari",hidari),("migi",migi)
        ,("ue",ue),("sita",sita),("iku",iku),("akeru",akeru)]

toMana :: String -> Maybe Mana
toMana str =
  let (s,m) = unzip manas
      i = if (elem str s) then getIndex str s else (-1)
   in if (i==(-1)) then Nothing else Just (m!!i)

tamanegi :: Mana
tamanegi = Mana (T ["tamanegi"] (Zai 1 Ko)) addOrdR

tamanegiP :: Mana
tamanegiP = Mana (T ["tamanegi"] (Con False Pa (rep 4 tamanegi))) addOrdR

reizouko :: Mana
reizouko = Mana (T ["reizouko"] (Box False Re reiT)) addOrd 

watasi :: Mana
watasi = Mana (T ["watasi"] (Wts (2,1) (2,1) [] [] [reizouko] [])) doNothing 

miki :: Mana
miki = Mana (T ["miki"] Pla) newRch 

hidari :: Mana
hidari = Mana (T ["hidari"] Pla) newRch 

migi :: Mana
migi = Mana (T ["migi"] Pla) newRch 

ue :: Mana
ue = Mana (T ["ue"] Pla) newRch 

sita :: Mana
sita = Mana (T ["sita"] Pla) newRch 

iku :: Mana
iku = Mana (T ["iku"] Dou) going

akeru :: Mana
akeru = Mana (T ["akeru"] Dou) openY

doNothing :: Y
doNothing t _ = t

going :: Y
going t@(T na (Wts pfr _ l r mns rc)) _ =
  let ds = if (length na<2) then "" else last na
      nps = searchRegion ds
   in if (ds=="") then t else T (init na) (Wts pfr nps l r mns rc)
going t1 _ = t1 

addOrd :: Y
addOrd (T na1 ta) (T na2 _) = T (na1++na2) ta

addOrdR :: Y
addOrdR t@(T na ta@(Wts _ _ _ _ _ rc)) (T na2 _) =
  if(elem (head na2) rc) then T (na++na2) ta else t
addOrdR t _ = t

openY :: Y
openY t@(T na@(_:ord) (Wts pfr pto l r mns rc)) _ =
  let ds = if (ord==[]) then "" else last ord
      iex = elem ds rc 
      tmi = if (ds=="" || iex==False) then (-1) else searchMana ds mns
      (nmn,nrc) = if (tmi==(-1) || tmi==length ds) then (mns,rc) 
                      else let (tt,ty) = getTY (mns!!tmi)
                               rc' = getRch tt
                            in (replCon tmi (Mana (openT tt) ty) mns,rc')
   in if (tmi==(-1)) then t else (T (na++["miki"]) (Wts pfr pto l r nmn nrc))
openY t _ = t

newRch :: Y
newRch t@(T (n:ord) (Wts pfr pto l r mns rc)) (T na2 _) =
  let tg = head na2
      ism = elem tg ord
      ord' = if ism then iniEL tg ord else ord 
   in if(elem tg rc) then
    let ths = sepEL "miki" ord' 
        ds = last$take (length ord' - length ths - 1) ord'
        tmi = searchMana ds mns
        addr = if ism then [] else ["miki"]
        nrc = let (tt,_) = getTY (mns!!tmi)
               in addr++getFStr (ths++na2) (getMnT tt)
        na2' = if ism then [] else na2
     in T ([n]++ord'++na2') (Wts pfr pto l r mns nrc)
                         else t
newRch t _ = t

getMnT :: T -> Tr Mana
getMnT (T _ (Box _ _ mnt)) = mnt
getMnT _ = Nd miki []

searchMana :: String -> [Mana] -> Int 
searchMana _ [] = 0 
searchMana ds ((Mana (T na _) _):ms) = if (ds==head na) then 0 else 1+searchMana ds ms

openT :: T -> T
openT (T na (Con _ c mns)) = T na (Con True c mns)
openT (T na (Box _ bt mnt)) = T na (Box True bt mnt)
openT t = t

