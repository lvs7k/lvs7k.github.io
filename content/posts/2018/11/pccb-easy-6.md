---
title: "Haskellで蟻本（初級編） - 数学的な問題を解くコツ"
date: 2018-11-27T22:42:23+09:00
draft: false
categories: ["competitive"]
tags: ["haskell"]
---

## 参考資料

- [大学数学へのかけ橋！『高校数学＋α ：基礎と論理の物語』](http://tad311.xsrv.jp/hsmath/)
    - 文系社会人が高校数学やり直すために何冊か本買ったけどこれが一番好き
    - PDFが無料でダウンロードできます

----

※蟻本の入力例でしかテストしていません


## ユークリッド互除法

```haskell
{-# LANGUAGE BangPatterns #-}

import qualified Data.IntMap.Strict as IM

-- ユークリッド互除法
gcd' :: Int -> Int -> Int
gcd' x y = go x y
  where
    go a 0 = a
    go a b = go b (a `mod` b)

-- 線分上の格子点の個数
q1 :: (Int, Int) -> (Int, Int) -> Int
q1 (x1, y1) (x2, y2) = gcd' (abs $ x1 - x2) (abs $ y1 - y2) - 1
```

- 参考資料の「1.10.2.1 ユークリッドの互除法」を読む
- 証明も理解する


### 拡張ユークリッド互除法

```haskell
-- 拡張ユークリッド互除法
extgcd :: Int -> Int -> (Int, Int)
extgcd x y = go x y
  where
    go a 0 = (1, 0)
    go a b = (t, s - q * t)
      where
        (q, r) = a `divMod` b
        (s, t) = extgcd b r

-- 双六
q2 :: Int -> Int -> Maybe (Int, Int, Int, Int)
q2 a b
    | gcd' a b == 1 = case () of
        _ | x < 0, y < 0 -> Just (0, 0, abs x, abs y)
          | x < 0        -> Just (0, y, abs x, 0)
          | y < 0        -> Just (x, 0, 0, abs y)
          | otherwise    -> Just (x, y, 0, 0)
    | otherwise     = Nothing
  where
    (x, y) = extgcd a b
```

- 拡張ユークリッド互除法で何ができるの？
    - `ax + by = gcd(a, b)`の整数解、`(x, y)`を求めることができる

- 上記のプログラムが理解できないんだけど？（蟻本の解説本が欲しいんだけど？）

    ```
    参考資料の「1.10.3 整数論の基本定理」を読む
    整数a，b が互いに素のとき ax + by = 1 は整数解をもつ
    整数a，b が互いに素のとき ax + by = c は整数解をもつ
    整数a, b が互いに素でなくても c が gcd(a, b) で割り切れるなら ax + by = c は整数解をもつ
    ということはax + by = gcd(a, b) は整数解をもつ
    a を b で割って a = bq + r として上に式に代入すると
    b(qx + y) + rx = gcd(a, b)
    s = qx + y, x = t とおいて bs + rt = gcd(s, t) = gcd(a, b)
    (b, r) は (a, b) よりも小さいため再帰的に解くことができる
    (s, t) がわかっているなら上の式で元の解(x, y) がわかる
    停止条件は r == 0 のとき bs + 0t = gcd(b, 0) の解(s, t) = (1, 0)
    ```

----

## 素数に関する基本的なアルゴリズム

```haskell
-- 素数判定
isPrime :: Int -> Bool
isPrime n = all ((/= 0) . (n `mod`)) $ takeWhile ((<= n) . (^ 2)) (primes n)

-- n以下の素数
primes :: Int -> [Int]
primes n = 2 : 3 : filter isPrime xs
  where
    xs = takeWhile (<= n) [6 * i + j | i <- [1 ..], j <- [-1, 1]]

-- 約数の列挙
divisor :: Int -> [Int]
divisor n = foldr f [] $ takeWhile ((<= n) . (^ 2)) [1 .. n]
  where
    f x ds
        | r == 0, q /= x = x : q : ds
        | r == 0         = x : ds
        | otherwise      = ds
      where
        (q, r) = n `divMod` x

-- 素因数分解
primeFactor :: Int -> IM.IntMap Int
primeFactor n = go IM.empty n $ takeWhile ((<= n) . (^ 2)) (primes n)
  where
    go im 1  _  = im
    go im !k [] = IM.insertWith (+) k 1 im
    go im !k (p:ps)
        | r == 0    = go (IM.insertWith (+) p 1 im) q (p:ps)
        | otherwise = go im k ps
      where
        (q, r) = k `divMod` p
```


### エラトステネスの篩

```haskell
-- エラトステネスの篩
sieve :: Int -> [Int]
sieve n = takeWhile (<= n) (sieve' [2 ..])
  where
    sieve' (x:xs) = x : sieve' (filter ((/= 0) . (`mod` x)) xs)

-- 区間内の素数の個数
q5 :: Int -> Int -> Int
q5 a b = go [a .. b - 1] (takeWhile ((< b) . (^ 2)) [2 ..])
  where
    go !ps []     = length ps
    go !ps (x:xs) = go (f ps) (f xs)
      where
        f as = filter ((/= 0) . (`mod` x)) as
```

- Haskellっぽくてかっこいいけど遅い、多分TLE

----

## べき乗を高速に計算する

```haskell
-- べき乗を高速に計算する
modPow' :: Int -> Int -> Int -> Int
modPow' x n m = go 1 x n
  where
    go !k !_ 0 = k
    go !k !p q
        | r == 1    = go (k * p `mod` m) (p ^ 2 `mod` m) q'
        | otherwise = go k               (p ^ 2 `mod` m) q'
      where
        (q', r) = q `divMod` 2

modPow :: Int -> Int -> Int -> Int
modPow x n m = go n
  where
    go 0 = 1
    go k
        | odd k     = (t * x) `mod` m
        | otherwise = t
      where
        s = go (k `div` 2)
        t = (s * s) `mod` m

-- Carmichael Numbers (UVa No.10006)
q6 :: Int -> Bool
q6 n
    | isPrime n = False
    | otherwise = all f [2 .. n - 1]
  where
    f x = modPow x n n == x `mod` n
```

- `modPow`のほうが分かりやすくて好き