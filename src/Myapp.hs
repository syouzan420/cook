module Myapp(appMain) where

import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.IORef(IORef,newIORef,readIORef,writeIORef)
import Control.Concurrent.Timer(repeatedTimer,stopTimer)
import Control.Concurrent.Suspend(msDelay)
import Mydata


appMain :: IO ()
appMain = do
  hSetBuffering stdout NoBuffering
  wts <- newIORef watasi
  t <- repeatedTimer (timerR wts) (msDelay 2000)
  appLoop wts
  stopTimer t

appLoop :: IORef Mana -> IO ()
appLoop wts = do
  com <- putStr "> " >> getLine
  w <- readIORef wts
  if (com=="exit") then return () else do
        let w' = exeCom com w
        case w' of
            Nothing -> putStrLn "Error!"
            Just jw -> do
              putStrLn (showW jw)
              writeIORef wts jw
        appLoop wts

exeCom :: String -> Mana -> Maybe Mana
exeCom com w = 
  let coms = words com
      mns = map toMana coms
   in foldl (.>) (Just w) mns 

doWTime :: Mana -> Mana
doWTime (Mana t@(T na (Wts pfr pto l r mns _)) y) = 
  if (pfr==pto) then Mana t y
                else let pfr' = moving pfr pto
                         rc' = posToRch pfr'
                      in Mana (T na (Wts pfr' pto l r mns rc')) y
doWTime mn = mn

moving :: Pos -> Pos -> Pos
moving (x1,y1) (x2,y2)
  |x2 > x1 = (x1 + 1, y1)
  |x1 > x2 = (x1 - 1, y1)
  |y2 > y1 = (x1, y1 + 1)
  |y1 > y2 = (x1, y1 - 1)
  |otherwise = (x1, y1)

showW :: Mana -> String
showW m@(Mana (T _ (Wts _ _ _ _ mns rc)) _) = 
  show m ++"\n" ++ concat (map (\mn -> show mn ++ "\n") mns) ++"reachable:"++show rc ++ "\n"
showW m = show m

timerR :: IORef Mana -> IO ()
timerR wts = do
  w <- readIORef wts
  let w' = doWTime w 
  if (w/=w') then putStrLn ("\n"++(show w')) >> putStr "> " else return ()
  writeIORef wts w'
