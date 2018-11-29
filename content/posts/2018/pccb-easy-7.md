---
title: "Haskellで蟻本（初級編） - GCJの問題に挑戦してみよう（１）"
date: 2018-11-29T22:06:10+09:00
draft: false
categories: ["competitive"]
tags: ["haskell"]
---

※蟻本の入力例でしかテストしていません

下2つはコンテストで出たとして解けなそう…。


## Minimum Scalar Product

```haskell
{-# LANGUAGE BangPatterns, FlexibleContexts #-}

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.STRef
import Data.List

-- Minimum Scalar Product (2008 Round1A A)
q1 :: Int -> [Int] -> [Int] -> Int
q1 n xs ys = sum $ zipWith (*) xs' ys'
  where
    xs' = sort xs
    ys' = sortBy (flip compare) ys
```

- 蟻本の解説参照

----

## Crazy Rows

```haskell
-- Crazy Rows (2009 Round2 A)
q2 :: Int -> [String] -> Int
q2 n xss = runST $ do
    a <- newListArray (1, n) (fmap last1 xss) :: ST s (STUArray s Int Int)
    ans <- newSTRef 0
    forM_ [1 .. n] $ \i -> do
        ai <- readArray a i
        when (ai > i) $ do
            js <- flip filterM [i + 1 .. n] $ \j -> do
                aj <- readArray a j
                return $ aj <= i
            when (not $ null js) (swap ans a i (head js))
    readSTRef ans
  where
    last1 xs = length $ dropWhile (/= '1') (reverse xs)
    swap ans a i j
        | i == j    = return ()
        | otherwise = do
            aj  <- readArray a j
            writeArray a j =<< readArray a (j - 1)
            writeArray a (j - 1) aj
            modifySTRef' ans (+ 1)
            swap ans a i (j - 1)
```

- `MArray`面倒くさい

----

## Bribe the Prisoners

```haskell
-- Bribe the Prisoners (2009 Round 1C C)
q3 :: Int -> Int -> [Int] -> Int
q3 p q as = runST $ do
    dp <- newArray ((0, 0), (q + 1, (q + 1))) maxBound :: ST s (STUArray s (Int, Int) Int)
    sequence_ [writeArray dp (i, i + 1) 0 | i <- [0 .. q]]
    forM_ [2 .. q + 1] $ \w -> do
        forM_ [0 .. (q + 1) - w] $ \i -> do
            let j = i + w
            t <- fmap minimum $ forM [i + 1 .. j - 1] $ \k -> do
                dpik <- readArray dp (i, k)
                dpkj <- readArray dp (k, j)
                return (dpik + dpkj)
            writeArray dp (i, j) (t + a ! j - a ! i - 1 - 1)
    readArray dp (0, q + 1)
  where
    a = listArray (0, q + 1) (0 : as ++ [p + 1])
```

- `dp[i][j] := (i, j)を解放するのに必要な金貨`
    - `例2) P = 20, Q = 3, A = {3, 6, 14}`
        - 配列Aを作る時に両端を追加して`A = [0, 3, 6, 14, 21]`
        - 例えば`dp[0][3]`は左端と囚人14は解放済みとして、囚人3と囚人6を解放するのに必要な金貨の最小枚数
- まず幅が1の時を0で初期化する
    - 幅が1、例えば`dp[2][3]`は間に解放すべき囚人がいないため金貨不要
- 幅が2（間に1人解放すべき囚人が存在する）から幅Q+1までループ
- 最初に解放する囚人をすべて試し、最小コストのものを探す
    - 例えば`dp[0][3]`は`dp[0][1] + dp[1][3]`と`dp[0][2] + dp[2][3]`の小さい方
    - 先に囚人3と囚人6のどちらを解放するにしても`14 - 0 - 1 - 1`の金貨は必要

----

## Millionaire

```haskell
-- Millionaire (2008 APAC local onsites C)
q4 :: Int -> Double -> Int -> Double
q4 m p x = go m gx
  where
    m2 = 2 ^ m
    go 0 j
        | j == m2   = 1.0
        | otherwise = 0.0
    go i j = maximum $ do
        v <- [0 .. min j (m2 - j)]
        return $ p * (memo ! (i - 1, j + v)) + (1 - p) * (memo ! (i - 1, j - v))
    memo = listArray ((0, 0), (m, m2)) [go i j | i <- [0 .. m], j <- [0 .. m2]]
    gx = (x * m2) `div` 1000000
```

- 例として`M = 2, P = 0.5, X = 500000`を考える
- `dp[i][j] := 残りラウンドがiで、所持金がグループjのとき、最善の戦略をとってお金を持って帰れる確率`
- 所持金のグループ
    - ラウンド数が2のとき
        - `j = 0,         0 ~ 249,999`
        - `j = 1,   250,000 ~ 499,999`
        - `j = 2,   500,000 ~ 749,999`
        - `j = 3,   750,000 ~ 999,999`
        - `j = 4, 1,000,000 ~`
- 配列を埋め終わった状態はこんな感じになる

    ```
            [0]    [1]    [2]    [3]    [4]
    dp[0] | 0.00 | 0.00 | 0.00 | 0.00 | 1.00 |
    dp[1] | 0.00 | 0.00 | 0.50 | 0.50 | 1.00 |
    dp[2] | 0.00 | 0.25 | 0.50 | 0.75 | 1.00 |
    ```

- 例えば`dp[1][2]`を更新するときはどうなるか
    - イメージ的には`dp[1][2]`から見てV字の先の値が必要になって、掛けた金額に応じてV字の角度が変化する（意味不明）
    - 掛けた金額が0なら、勝とうが負けようが`dp[0][2]`の状態になって
    - 全額かけたなら、勝てば`dp[0][4]`の状態になって負ければ`dp[0][0]`の状態になって
    - その間の金額をかけたら、`dp[0][1]`, `dp[0][3]`みたいな…
- 最初に持っている金額500,000はグループ2なので答えは`dp[2][2]`の`0.50`
    - 蟻本の`int i = (ll)X * n / 1000000;`の部分で最初の所持金がどのグループか判別している
    - `500,000 / (1,000,000 / 2^2) == 500,000 * 2^2 / 1,000,000 == 2`