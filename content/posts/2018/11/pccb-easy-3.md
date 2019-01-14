---
title: "Haskellで蟻本（初級編） - 動的計画法"
date: 2018-11-19T22:09:54+09:00
draft: false
categories: ["competitive"]
tags: ["haskell"]
---

※蟻本の入力例でしかテストしていません


```haskell
{-# LANGUAGE BangPatterns, FlexibleContexts #-}

import Control.Monad
import Data.List
import qualified Data.ByteString.Char8 as B

import Control.Monad.ST
import Data.Array.ST
import Data.Array
import qualified Data.Map.Strict as M

-- 01 ナップサック問題
q1 :: Int -> Int -> [(Int, Int)] -> Int
q1 n m wvs = go n m
  where
    wva = listArray (1, n) wvs
    memo = listArray ((0, 0), (n, m)) [go i j | i <- [0 .. n], j <- [0 .. m]]
    go 0 _ = 0
    go i j
        | j < w     = x1
        | otherwise = max x1 x2
      where
        (w, v) = wva ! i
        x1 = memo ! (i - 1, j)
        x2 = memo ! (i - 1, j - w) + v

q1' :: Int -> Int -> [(Int, Int)] -> Int
q1' n m wvs = runST $ do
    dp <- newArray ((0, 0), (n, m)) 0 :: ST s (STUArray s (Int, Int) Int)
    forM_ [1 .. n] $ \i -> do
        let (w, v) = wva ! i
        forM_ [0 .. m] $ \j -> do
            x1 <- readArray dp (i - 1, j)
            if j < w
                then writeArray dp (i, j) x1
                else do
                    x2 <- readArray dp (i - 1, j - w)
                    writeArray dp (i, j) (max x1 (x2 + v))
    readArray dp (n, m)
  where
    wva = listArray (1, n) wvs

q1'' :: Int -> Int -> [(Int, Int)] -> Int
q1'' n m wvs = runST $ do
    dp   <- newArray ((0, 0), (n, m)) 0     :: ST s (STUArray s (Int, Int) Int)
    memo <- newArray ((0, 0), (n, m)) False :: ST s (STUArray s (Int, Int) Bool)
    sequence_ [writeArray memo (0, i) True | i <- [0 .. m]]
    go dp memo (n, m)
  where
    wva = listArray (1, n) wvs
    go dp memo (i, j) = do
        exists <- readArray memo (i, j)
        when (not exists) $ do
            let (w, v) = wva ! i
            x1 <- go dp memo (i - 1, j)
            if j < w
                then do
                    writeArray dp (i, j) x1
                else do
                    x2 <- go dp memo (i - 1, j - w)
                    writeArray dp (i, j) (max x1 (x2 + v))
            writeArray memo (i, j) True
        readArray dp (i, j)
```

- メモ化再帰　これでAC取りたい
- 動的計画法　圧倒的速さ
- メモ化再帰　上のでTLEになるがメモ化再帰したいとき？


```haskell
-- 最長共通部分列問題
q2 :: Int -> Int -> B.ByteString -> B.ByteString -> Int
q2 n m ss ts = go (n, m)
  where
    memo = listArray ((0, 0), (n, m)) [go (i, j) | i <- [0 .. n], j <- [0 .. m]]
    go (0, _) = 0
    go (_, 0) = 0
    go (i, j)
        | si == tj  = maximum [x1, x2, x3]
        | otherwise = max x1 x2
      where
        si = ss `B.index` (i - 1)
        tj = ts `B.index` (j - 1)
        x1 = memo ! (i - 1, j)
        x2 = memo ! (i, j - 1)
        x3 = memo ! (i - 1, j - 1) + 1
```

- `ByteString`の`index`は`O(1)`
- `where`句に細かく名前つけてあげると読みやすくなる気がする


```haskell
-- 個数制限なしナップサック問題
q3 :: Int -> Int -> [(Int, Int)] -> Int
q3 n m wvs = go (n, m)
  where
    wva  = listArray (1, n) wvs
    memo = listArray ((0, 0), (n, m)) [go (i, j) | i <- [0 .. n], j <- [0 .. m]]
    go (0, _) = 0
    go (i, j)
        | j < w     = x1
        | otherwise = max x1 x2
      where
        (w, v) = wva ! i
        x1 = memo ! (i - 1, j)
        x2 = memo ! (i, j - w) + v
```

- このように計算量を減らすのだって書いてあるけど思いつけるようになるのだろうか


```haskell
-- 01 ナップサック問題 その2
q4 :: Int -> Int -> [(Int, Int)] -> Int
q4 n m wvs = maximum $ do
    v <- [0 .. vmax]
    let w = memo ! (n, v)
    guard (w <= m)
    return v
  where
    inf = 10 ^ 10
    vmax = sum $ fmap snd wvs
    wva = listArray (1, n) wvs
    memo = listArray ((0, 0), (n, vmax)) [go (i, j) | i <- [0 .. n], j <- [0 .. vmax]]
    go (0, 0) = 0
    go (0, _) = inf
    go (i, j)
        | j < v     = x1
        | otherwise = min x1 x2
      where
        (w, v) = wva ! i
        x1 = go (i - 1, j)
        x2 = go (i - 1, j - v) + w
```


```haskell
-- 個数制限つき部分和問題
q5 :: Int -> Int -> [Int] -> [Int] -> Bool
q5 n k as ms
    | go (n, k) < 0 = False
    | otherwise     = True
  where
    aa = listArray (1, n) as
    ma = listArray (1, n) ms
    memo = listArray ((0, 0), (n, k)) [go (i, j) | i <- [0 .. n], j <- [0 .. k]]
    go (0, 0) = 0
    go (0, _) = -1
    go (i, j)
        | x1 >= 0          = m
        | j < a || x2 <= 0 = -1
        | otherwise        = x2 - 1
      where
        a = aa ! i
        m = ma ! i
        x1 = memo ! (i - 1, j)
        x2 = memo ! (i, j - a)
```


```haskell
-- 最長増加部分列問題
q6 :: Int -> [Int] -> Int
q6 n as = go n
  where
    aa = listArray (1, n) as
    memo = listArray (1, n) [go i | i <- [1 .. n]]
    go 1 = 1
    go i = maximum . (1:) $ do
        j <- [1 .. i - 1]
        let (ai, aj) = (aa ! i, aa ! j)
        guard (aj < ai)
        return $ (memo ! j) + 1

q6' :: Int -> [Int] -> Int
q6' n as = maximum $ foldl' f M.empty as
  where
    f m a
        | Just (k, v) <- M.lookupGE a m = M.insert a v $ M.delete k m
        | otherwise                     = M.insert a (M.size m + 1) m
```

- `IntMap`と異なり`Map`の`size`は`O(1)`
- 蟻本の「DPテーブルの変化」の図と同じように`Map`が更新されていく


```haskell
-- 分割数
q7 :: Int -> Int -> Int -> Int
q7 n m d = go (m, n)
  where
    memo = listArray ((0, 0), (m, n)) [go (i, j) | i <- [0 .. m], j <- [0 .. n]]
    go (0, 0) = 1
    go (0, _) = 0
    go (i, j)
        | j < i     = x1
        | otherwise = (x1 + x2) `mod` d
      where
        x1 = memo ! (i - 1, j)
        x2 = memo ! (i, j - 1)
```

```
漸化式がよく理解できなかったのでメモ

dp[3][4] = 4 (4の3分割の総数)

  (4, 0, 0)
  (3, 1, 0) -- dp[2][4]と同じ
  (2, 2, 0)

  (2, 1, 1) -- 4の3分割 (全て1以上) == (4 - 3)の3分割 (全て0以上)
```


```haskell
-- 重複組合せ
q8 :: Int -> Int -> Int -> [Int] -> Int
q8 n m d as = go (n, m)
  where
    aa = listArray (1, n) as
    memo = listArray ((0, 0), (n, m)) [go (i, j) | i <- [0 .. n], j <- [0 .. m]]
    go (0, 0) = 1
    go (0, _) = 0
    go (i, j)
        | j - 1 < 0      = x1
        | j - 1 - ai < 0 = (x1 + x2) `mod` d
        | otherwise      = (x1 + x2 - x3) `mod` d
      where
        ai = aa ! i
        x1 = memo ! (i - 1, j)
        x2 = memo ! (i, j - 1)
        x3 = memo ! (i - 1, j - 1 - ai)
```

- [重複組み合わせ - れどこだ目指すよ！　(；`・ω・）](http://d.hatena.ne.jp/phyllo_algo/20140831/1409502655)
    - 上の図と下の図の矢印が繋がっている□を見比べる
    - `X = C + B`だと`A`が余計に足されているので引いてあげる