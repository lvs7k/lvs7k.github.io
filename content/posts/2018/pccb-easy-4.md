---
title: "Haskellで蟻本（初級編） - データ構造"
date: 2018-11-20T21:01:17+09:00
draft: false
categories: ["competitive"]
tags: ["haskell"]
---

※蟻本の入力例でしかテストしていません


## 優先度付きキュー

> 多くのプログラミング言語では効率的に実装されたプライオリティキューが標準で含まれています。

と書かれているが、Haskellには標準で含まれていない。

じゃあどうするかというと

1. `containers`の`Map`か`Set`の`Min/Max`の関数で頑張る
2. 自分で作る

の2択になると思う。

----

## Leftist Heap

```haskell
module Heap
    ( Heap (..)
    , meld
    , push
    , pop
    ) where

import Control.Monad

data Heap a
    = Empty
    | Node !Int !a !(Heap a) !(Heap a)
    deriving Show

rank :: Heap a -> Int
rank Empty          = 0
rank (Node s _ _ _) = s

heap :: a -> Heap a -> Heap a -> Heap a
heap x a b
    | ra < rb   = Node (ra + 1) x b a
    | otherwise = Node (rb + 1) x a b
  where
    ra = rank a
    rb = rank b

meld :: Ord a => Heap a -> Heap a -> Heap a
meld a     Empty = a
meld Empty b     = b
meld a@(Node _ ax al ar) b@(Node _ bx bl br)
    | ax < bx   = heap ax al (meld ar b)
    | otherwise = heap bx bl (meld br a)

push :: Ord a => a -> Heap a -> Heap a
push x a = meld a (Node 1 x Empty Empty)

pop :: Ord a => Heap a -> Maybe (a, Heap a)
pop Empty          = Nothing
pop (Node _ x l r) = Just (x, meld l r)

test :: (Show a, Ord a) => [a] -> IO ()
test xs = do
    heap <- flip (flip foldM Empty) xs $ \h i -> do
        let h' = push i h
        print h'
        return h'
    putStrLn "--------------------"
    flip (flip foldM_ heap) [1 .. length xs] $ \h _ -> do
        print h
        let Just (_, h') = pop h
        return h'
```

```
*Heap> test [4,1,2,5,3]
Node 1 4 Empty Empty
Node 1 1 (Node 1 4 Empty Empty) Empty
Node 2 1 (Node 1 4 Empty Empty) (Node 1 2 Empty Empty)
Node 2 1 (Node 1 4 Empty Empty) (Node 1 2 (Node 1 5 Empty Empty) Empty)
Node 2 1 (Node 2 2 (Node 1 5 Empty Empty) (Node 1 3 Empty Empty)) (Node 1 4 Empty Empty)
--------------------
Node 2 1 (Node 2 2 (Node 1 5 Empty Empty) (Node 1 3 Empty Empty)) (Node 1 4 Empty Empty)
Node 2 2 (Node 1 5 Empty Empty) (Node 1 3 (Node 1 4 Empty Empty) Empty)
Node 2 3 (Node 1 4 Empty Empty) (Node 1 5 Empty Empty)
Node 1 4 (Node 1 5 Empty Empty) Empty
Node 1 5 Empty Empty
```

----

## Skew Heap

- Leftist Heapから木のバランスを取る処理を除いたもの
- 実装が軽いのに速さもそんな変わらないというのをどこかで見た
- 大きい順に取り出したいときは要修正

```haskell
module Skew
    ( Skew (..)
    , meld
    , push
    , pop
    ) where

import Control.Monad

data Skew a
    = Empty
    | Node !a !(Skew a) !(Skew a)
    deriving Show

meld :: Ord a => Skew a -> Skew a -> Skew a
meld a     Empty = a
meld Empty b     = b
meld a@(Node ax al ar) b@(Node bx bl br)
    | ax < bx   = Node ax al (meld ar b)
    | otherwise = Node bx bl (meld br a)

push :: Ord a => a -> Skew a -> Skew a
push x a = meld a (Node x Empty Empty)

pop :: Ord a => Skew a -> Maybe (a, Skew a)
pop Empty        = Nothing
pop (Node x l r) = Just (x, meld l r)

test :: (Show a, Ord a) => [a] -> IO ()
test xs = do
    skew <- flip (flip foldM Empty) xs $ \h i -> do
        let h' = push i h
        print h'
        return h'
    putStrLn "--------------------"
    flip (flip foldM_ skew) [1 .. length xs] $ \h _ -> do
        print h
        let Just (_, h') = pop h
        return h'
```

```
*Skew> test [4,1,2,5,3]
Node 4 Empty Empty
Node 1 Empty (Node 4 Empty Empty)
Node 1 Empty (Node 2 Empty (Node 4 Empty Empty))
Node 1 Empty (Node 2 Empty (Node 4 Empty (Node 5 Empty Empty)))
Node 1 Empty (Node 2 Empty (Node 3 Empty (Node 4 Empty (Node 5 Empty Empty))))
--------------------
Node 1 Empty (Node 2 Empty (Node 3 Empty (Node 4 Empty (Node 5 Empty Empty))))
Node 2 Empty (Node 3 Empty (Node 4 Empty (Node 5 Empty Empty)))
Node 3 Empty (Node 4 Empty (Node 5 Empty Empty))
Node 4 Empty (Node 5 Empty Empty)
Node 5 Empty Empty
```

----

## プライオリティキューを用いる問題

```haskell
-- Expedition (POJ 2431)
q1 :: Int -> Int -> Int -> [Int] -> [Int] -> Int
q1 n l p as bs = go 0 p Empty (zip bs as)
  where
    go !k fuel pque bas
        | fuel >= l    = k
        | Just ((g, _), pque'') <- pop pque'
            = go (k + 1) (fuel + g) pque'' ys
        | otherwise = -1
      where
        (xs, ys) = span ((<= fuel) . snd) bas
        pque' = foldr push pque xs
```

- fuelがゴールまで足りていたら補給回数を返す
- そうでなければ通り過ぎたガソリンスタンドを全てキューに追加し、補給できる量が最大のものを取り出す
- 取り出せた場合は再帰、取り出せなかったら-1


```haskell
-- Fence Repair (POJ 3253)
q2 :: Int -> [Int] -> Int
q2 n ls = go 0 (foldr push Empty ls)
  where
    go !k s
        | Just (x1, s1) <- pop s
        , Just (x2, s2) <- pop s1
            = let x = x1 + x2 in go (k + x) (push x s2)
        | otherwise = k
```

----

## Union-Find木

- 経路圧縮のみ

```haskell
module UF
    ( UF (..)
    , newUF
    , root
    , same
    , unite
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST

newtype UF s
    = UF (STUArray s Int Int)

newUF :: (Int, Int) -> ST s (UF s)
newUF (i, j) = UF <$> (newListArray (i, j) [i .. j])

root :: UF s -> Int -> ST s Int
root (UF a) i = do
    p <- readArray a i
    if p == i
        then return p
        else do
            rp <- root (UF a) p
            writeArray a i rp
            return rp

same :: UF s -> Int -> Int -> ST s Bool
same uf x y = (==) <$> (root uf x) <*> (root uf y)

unite :: UF s -> Int -> Int -> ST s ()
unite uf@(UF a) i j = do
    ri <- root uf i
    rj <- root uf j
    when (ri /= rj) $ do
        writeArray a rj ri
```

----

## Union-Find木を用いる問題

```haskell
-- 食物連鎖 (POJ 1182)
q3 :: Int -> Int -> [(Int, Int, Int)] -> Int
q3 n k qs = runST $ do
    uf <- newUF (1, n * 3)
    go 0 uf qs
  where
    go !f _  [] = return f
    go !f uf ((1, x, y):rs) = do
        let p1 = or [x < 1, y < 1, x > n, y > n]
        p2 <- (||) <$> same uf x (y + n) <*> same uf x (y + n * 2)
        if p1 || p2
            then go (f + 1) uf rs
            else do
                unite uf x y
                unite uf (x + n) (y + n)
                unite uf (x + n * 2) (y + n * 2)
                go f uf rs
    go !f uf ((2, x, y):rs) = do
        let p1 = or [x < 1, y < 1, x > n, y > n]
        p2 <- (||) <$> same uf x y <*> same uf x (y + n * 2)
        if p1 || p2
            then go (f + 1) uf rs
            else do
                unite uf x (y + n)
                unite uf (x + n) (y + n * 2)
                unite uf (x + n * 2) y
                go f uf rs
```

- `n = 100`のときUnion-Findの要素数はその3倍として
    - `1: 1が種類A`, `101: 1が種類B`, `201: 1が種類C`
    - `2: 2が種類A`, `102: 2が種類B`, `202: 2が種類C`
    - `...`

- 与えられた情報より、同時に起こるものを同じグループとしていく
    - `情報1: 1と2は同じ種類です。`
        - `unite uf 1 2`, `unite uf 101 102`, `unite uf 201 202`
    - `情報2: 1と2を食べます。`
        - `unite uf 1 102`, `unite uf 101 202`, `unite uf 201 2`