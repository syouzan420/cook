module Myapp(appMain) where

import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.IORef(IORef,newIORef,readIORef,writeIORef)
import Control.Concurrent.Timer(repeatedTimer)
import Control.Concurrent.Suspend(msDelay)
import Mydata


appMain :: IO ()
appMain = do
  hSetBuffering stdout NoBuffering
  wts <- newIORef watasi
  _ <- repeatedTimer (timerR wts) (msDelay 1000)
  appLoop wts

appLoop :: IORef Mana -> IO ()
appLoop wts = do
  w <- readIORef wts
  com <- putStr "> " >> getLine
  if (com=="exit") then return () else do
        let w' = exeCom com w
        writeIORef wts w'
        appLoop wts

exeCom :: String -> Mana -> Mana
exeCom = undefined

timerR :: IORef Mana -> IO ()
timerR wts = do
  w <- readIORef wts
  let w' = w
  writeIORef wts w'
