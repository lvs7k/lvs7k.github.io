---
title: "Haskellで蟻本（初級編） - 貪欲法"
date: 2018-11-18T12:39:46+09:00
draft: false
categories: ["competitive"]
tags: ["haskell"]
---

※蟻本の入力例でしかテストしていません


```haskell
{-# LANGUAGE BangPatterns, FlexibleContexts #-}

import Control.Monad
import Data.List

import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.Array         (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.IArray
import qualified Data.Set as S

-- 硬貨の問題
q1 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
q1 n c1 c5 c10 c50 c100 c500 = go 0 n cs
  where
    cs = zip [500, 100, 50, 10, 5, 1] [c500, c100, c50, c10, c5, c1]
    go !k a [] = k
    go !k a ((s, t):ds)
        | a >= s && t > 0 = go (k + 1) (a - s) ((s, t - 1):ds)
        | otherwise       = go k a ds
```


```haskell
-- 区間スケジューリング問題
q2 :: Int -> [(Int, Int)] -> Int
q2 n ss = go 0 (sortOn snd ss)
  where
    go !k [] = k
    go !k ((s, t):ts) = go (k + 1) (filter ((>= t) . fst) ts)
```


```haskell
-- Best Cow Line (POJ 3617)
q3 :: Int -> String -> String
q3 n ss = reverse $ go "" ss
  where
    go !ts "" = ts
    go !ts us
        | us <= us' = go (head us  : ts) (tail us)
        | otherwise = go (head us' : ts) (tail us')
      where
        us' = reverse us
```


```haskell
-- Saruman's Army (POJ 3069)
q4 :: Int -> Int -> [Int] -> Int
q4 n r xs = go 0 (sort xs)
  where
    go !k []     = k
    go !k (y:ys) = go (k + 1) cs
      where
        (as, bs) = span (<= y + r) (y:ys)
        cs = dropWhile (<= last as + r) bs
```


```haskell
-- Fence Repair (POJ 3253)
q5 :: Int -> [Int] -> Int
q5 n ls = go 0 n (S.fromList $ zip ls [1 ..])
  where
    go !k m s
        | Just ((v1, _), s')  <- S.minView s
        , Just ((v2, _), s'') <- S.minView s'
            = go (k + v1 + v2) (m + 1) (S.insert (v1 + v2, m + 1) s'')
        | otherwise = k
```

- `Data.Set`を優先度付きキューとして使用している
- 値が重複しないように`(<値>, <識別番号>)`を入れている
