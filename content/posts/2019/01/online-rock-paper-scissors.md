---
title: "Haskellでオンライン対戦じゃんけんゲーム"
date: 2019-01-02T23:05:03+09:00
draft: false
categories: ["programming"]
tags: ["haskell"]
---

## マルチスレッドプログラミング難しい

- `forkFinally`とか`withAsync`とか`race`とか`concurrently`とかどれを使えば良いのか
- 検索してもちょうどいい教材的なコードが見つからない
- `race_ (putStrLn =<< getLine) (return ())`が`getLine`の終了を待つのはなぜ？

## コード

- `nc localhost 1234`で起動
- バグあり
    - `Main.hs: <socket: 444>: commitBuffer: invalid argument (Invalid argument)`
    - なにこれ？

```haskell
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception
import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Unique
import Network
import System.IO
import Text.Printf
import qualified Data.Map.Strict as M

main :: IO ()
main = withSocketsDo $ do
  global <- newGlobal
  forkIO $ monitorClients global

  socket <- listenOn (PortNumber 1234)
  putStrLn "Listening on port 1234 ..."
  let loop = forever $ do
        (hdl, host, port) <- accept socket
        printf "Connection %s: %s\n" host (show port)
        forkFinally
          (clientMain global hdl)
          (\_ -> hPutStrLn hdl "disconnected (press enter key)" >> hClose hdl)
  loop `finally` (sClose socket)

--------------------------------------------------------------------------------

data Global = Global
  { clients :: TVar (M.Map Unique Client)
  }

newGlobal :: IO Global
newGlobal = Global <$> newTVarIO M.empty

addClient :: Global -> Client -> IO ()
addClient Global{..} client@Client{..} = atomically $ do
  modifyTVar' clients $ \m -> M.insert clientId client m

removeClient :: Global -> Client -> IO ()
removeClient Global{..} Client{..} = atomically $ do
  modifyTVar' clients $ \m -> M.delete clientId m

monitorClients :: Global -> IO ()
monitorClients Global{..} = do
  k <- M.size <$> readTVarIO clients
  loop k
 where
  loop k = join . atomically $ do
    m <- readTVar clients
    let k' = M.size m
    case () of
      _ | k  == k'  -> retry
        | k' == 2   -> return $ do
            let (c1:c2:_) = M.elems m
            matchMaking c1 c2
            loop k'
        | otherwise -> return $ do
            printf "There are %d player in room\n" k'
            loop k'

matchMaking :: Client -> Client -> IO ()
matchMaking c1 c2 = do
  a <- async $ fight c1 c2
  atomically $ do
    writeTChan (clientRecvChan c1) (Fight a)
    writeTChan (clientRecvChan c2) (Fight a)

--------------------------------------------------------------------------------

data Message
  = ClientInput String
  | ClientDisconnected
  | Fight (Async ())

readMessage :: Client -> IO Message
readMessage Client{..} = do
  msg <- atomically $ readTChan clientRecvChan
  case msg of
    ClientDisconnected -> throwIO (userError "disconnected")
    _                  -> return msg

--------------------------------------------------------------------------------

data Client = Client
  { clientId       :: Unique
  , clientHandle   :: Handle
  , clientRecvChan :: TChan Message
  }

clientMain :: Global -> Handle -> IO ()
clientMain global hdl = do
  hSetNewlineMode hdl universalNewlineMode
  hSetBuffering hdl LineBuffering
  client <- newClient hdl
  runClient client `finally` cleanUp client
 where
  runClient client = do
    addClient global client
    a1 <- async $ clientInputReceiver client
    a2 <- async $ clientRecvChanHandler client
    wait a2 `finally` forkIO (cancel a1)

  cleanUp client = do
    removeClient global client

clientInputReceiver :: Client -> IO ()
clientInputReceiver Client{..} = flip finally disconnect $ forever $ do
  line <- hGetLine clientHandle
  atomically $ writeTChan clientRecvChan (ClientInput line)
 where
  disconnect = do
    atomically $ writeTChan clientRecvChan ClientDisconnected

clientRecvChanHandler :: Client -> IO ()
clientRecvChanHandler client = loop
 where
  loop = do
    msg <- readMessage client
    case msg of
      ClientInput s -> putStrLn s >> loop
      Fight a       -> wait a

newClient :: Handle -> IO Client
newClient hdl = do
  u <- newUnique
  c <- newTChanIO
  return Client
    { clientId       = u
    , clientHandle   = hdl
    , clientRecvChan = c
    }

--------------------------------------------------------------------------------

data Hand
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Show)

instance Ord Hand where
  Rock     `compare` Paper    = LT
  Rock     `compare` Scissors = GT
  Paper    `compare` Scissors = LT
  Paper    `compare` Rock     = GT
  Scissors `compare` Rock     = LT
  Scissors `compare` Paper    = GT
  _        `compare` _        = EQ

fight :: Client -> Client -> IO ()
fight c1 c2 = loop
 where
  hdl1 = clientHandle c1
  hdl2 = clientHandle c2
  loop = do
    (h1, h2) <- concurrently (getHand c1) (getHand c2)
    hPrintf hdl1 "You: %s, Opponent: %s\n" (show h1) (show h2)
    hPrintf hdl2 "You: %s, Opponent: %s\n" (show h2) (show h1)
    case h1 `compare` h2 of
      LT -> do
        hPutStrLn hdl1 "You lose..."
        hPutStrLn hdl2 "You win!"
      GT -> do
        hPutStrLn hdl1 "You win!"
        hPutStrLn hdl2 "You lose..."
      EQ -> loop

getHand :: Client -> IO Hand
getHand client@Client{..} = loop
 where
  loop = do
    hPutStrLn clientHandle "1=Rock, 2=Paper, 3=Scissors:"
    (ClientInput k) <- readMessage client
    case k of
      '1':_ -> return Rock
      '2':_ -> return Paper
      '3':_ -> return Scissors
      _     -> loop
```
