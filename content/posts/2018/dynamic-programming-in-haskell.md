---
title: "Haskellで動的計画法（準備編）"
date: 2018-11-12T21:47:14+09:00
draft: false
categories: ["programming"]
tags: ["haskell"]
---


## 知りたいこと

- Haskellで動的計画法を書く時にどういう書き方をすればいいのか


## 試してみたこと

- `f(n) = f(n - 1) + 1`
- 初項1、公差1の等差数列の和を`n = 10 ^ 6`と`n = 10 ^ 7`で計算
- PowerShellの`Measure-Command`で時間を計測
- 時間計測時に`stack run`や`stack exec`を使うと200ms~400ms余計に時間がかかった


## 結論

- `f1`でTLEにならないなら`f1`
- ボトムアップに配列埋める場合は`f2`
- `f1`では間に合わないがメモ化再帰したい場合は`f3`
- 間違ったこと言ってたらごめんなさい


## コード

```haskell
{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Array
import Data.Array.ST
import qualified Data.IntMap.Strict as IMS
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont

-- f(n) = f(n - 1) + 1
-- stack build
-- Measure-Command { ./stack-work/~.exe | Out-Default } | Select-Object TotalMilliseconds

-- n = 10 ^ 6 :  477 ms
-- n = 10 ^ 7 : 4308 ms
f1 :: Int -> Int
f1 n = go n
  where
    memo = listArray (1, n) (fmap go [1 .. n])
    go 1 = 1
    go k = memo ! (k - 1) + 1
```

- メモ化再帰 (`Array`)
- これ以下の速度のものは使う必要がない

```haskell
-- n = 10 ^ 6 :  14 ms
-- n = 10 ^ 7 :  97 ms
f2 :: Int -> Int
f2 n = runST $ do
    a <- newArray (1, n) 1 :: ST s (STUArray s Int Int)
    forM_ [2 .. n] $ \i -> do
        b <- readArray a (i - 1)
        writeArray a i (b + 1)
    readArray a n
```

- 動的計画法 (`UArray`)
- 最速

```haskell
-- n = 10 ^ 6 :  159 ms
-- n = 10 ^ 7 : 1850 ms
f3 :: Int -> Int
f3 n = runST $ do
    a <- newArray (1, n) 1     :: ST s (STUArray s Int Int)
    m <- newArray (1, n) False :: ST s (STUArray s Int Bool)
    writeArray m 1 True
    go a m n
  where
    go :: STUArray s Int Int -> STUArray s Int Bool -> Int -> ST s Int
    go a m i = do
        evaluated <- readArray m i
        if evaluated
            then readArray a i
            else do
                b <- go a m (i - 1)
                writeArray a i (b + 1)
                writeArray m i True
                return (b + 1)
```

- メモ化再帰 (`UArray`)
- 関数呼び出しのオーバーヘッドなのか大分遅くなる

```haskell
-- n = 10 ^ 6 :  192 ms
-- n = 10 ^ 7 : 1654 ms
f4 :: Int -> Int
f4 n = runST $ do
    a <- newArray (1, n) 1     :: ST s (STUArray s Int Int)
    m <- newArray (1, n) False :: ST s (STUArray s Int Bool)
    writeArray m 1 True
    go a m n
  where
    go :: STUArray s Int Int -> STUArray s Int Bool -> Int -> ST s Int
    go a m i = evalContT . callCC $ \cc -> do
        evaluated <- lift $ readArray m i
        when evaluated $ do
            b <- lift $ readArray a i
            cc b
        c <- lift $ go a m (i - 1)
        lift $ writeArray a i (c + 1)
        lift $ writeArray m i True
        return (c + 1)
```

- 速さとは関係ないがインデントが深くなるのが嫌なら`Cont`モナドを使う

```haskell
-- n = 10 ^ 6 :  404 ms
-- n = 10 ^ 7 : 4369 ms
f5 :: Int -> Int
f5 n = runST $ do
    a <- newArray (1, n) Nothing :: ST s (STArray s Int (Maybe Int))
    writeArray a 1 (Just 1)
    go a n
    Just i <- readArray a n
    return i
  where
    go :: STArray s Int (Maybe Int) -> Int -> ST s Int
    go a i = do
        mi <- readArray a i
        case mi of
            Just b  -> return b
            Nothing -> do
                c <- go a (i - 1)
                writeArray a i (Just (c + 1))
                return (c + 1)
```

- メモ化再帰 (`Array`)
- 計算済みかどうかを`Array`に`Maybe Int`を入れて判断した場合

```haskell
-- n = 10 ^ 6 :  459 ms
-- n = 10 ^ 7 : 3615 ms
f6 :: Int -> Int
f6 n = (foldl' f im [2 .. n]) IMS.! n
  where
    im = IMS.singleton 1 1 :: IMS.IntMap Int
    f m i = let b = m IMS.! (i - 1)
            in IMS.insert i (b + 1) m
```

- 動的計画法 (`IntMap.Strict`)
- `Lazy`はこれより大分遅かったので省略

```haskell
-- n = 10 ^ 6 :  516 ms
-- n = 10 ^ 7 : 4380 ms
f7 :: Int -> Int
f7 n = (go im n) IMS.! n
  where
    im = IMS.singleton 1 1 :: IMS.IntMap Int
    go !m i
        | Just _ <- IMS.lookup i m = m
        | otherwise                = IMS.insert i (b + 1) m'
      where
        m' = go m (i - 1)
        b  = m' IMS.! (i - 1)
```

- メモ化再帰 (`IntMap.Strict`)
- `Lazy`はこれより大分遅かったので省略