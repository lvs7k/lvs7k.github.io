---
title: "Haskellで蟻本（初級編） - 全探索"
date: 2018-11-17T20:40:14+09:00
draft: false
categories: ["competitive"]
tags: ["haskell"]
---

※蟻本の入力例でしかテストしていません


```haskell
{-# LANGUAGE BangPatterns, FlexibleContexts #-}

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed (UArray)
import Data.Array.IArray
import Data.STRef
import qualified Data.Set as S

-- 部分和問題
q1 :: Int -> Int -> [Int] -> Bool
q1 n k as = go 0 as
  where
    go !i []
        | i == k    = True
        | otherwise = False
    go !i (b:bs) = go (i + b) bs || go i bs

q1' :: Int -> Int -> [Int] -> Bool
q1' n k as = go k as
  where
    go !i []
        | i == 0    = True
        | otherwise = False
    go !i (b:bs) = go (i - b) bs || go i bs

q1'' :: Int -> Int -> [Int] -> Bool
q1'' n k as = any (== k) $ do
    bs <- sequence (replicate n [True, False])
    let cs = [a | (a, True) <- zip as bs]
    return (sum cs)
```

- `q1''`はビットを使った状態の列挙


```haskell
-- Lake Counting (POJ No.2386)
q2 :: Int -> Int -> [String] -> Int
q2 n m xss = runST $ do
    as <- newListArray ((1, 1), (n, m)) (concat xss) :: ST s (STUArray s (Int, Int) Char)
    refc <- newSTRef 0
    forM_ [1 .. n] $ \i -> do
        forM_ [1 .. m] $ \j -> do
            c <- readArray as (i, j)
            when (c == 'W') $ do
                dfs as (i, j)
                modifySTRef' refc (+ 1)
    readSTRef refc
  where
    dfs as (i, j) = do
        c <- readArray as (i, j)
        case c of
            '.' -> return ()
            _   -> do
                writeArray as (i, j) '.'
                mapM_ (dfs as) next
      where
        next = do
            s <- [i - 1, i, i + 1]
            t <- [j - 1, j, j + 1]
            guard (inRange ((1, 1), (n, m)) (s, t))
            return (s, t)
```


```haskell
-- 迷路の最短路
q3 :: Int -> Int -> [String] -> Int
q3 n m xss = fst . head $ dropWhile ((/= goal) . snd) queue
  where
    (start, _) = head . filter ((== 'S') . snd)
               $ zip [(i, j) | i <- [1 .. n], j <- [1 .. m]] (concat xss)
    (goal, _)  = head . filter ((== 'G') . snd)
               $ zip [(i, j) | i <- [1 .. n], j <- [1 .. m]] (concat xss)
    maze = listArray ((1, 1), (n, m)) (concat xss) :: UArray (Int, Int) Char
    queue = (0, start) : go (S.singleton start) queue
    go s ((dist, (i, j)):qs) = next' ++ go s' qs
      where
        next  = filter f2 $ filter f1 [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
        next' = [(dist + 1, p) | p <- next]
        s' = foldr S.insert s next
        f1 pos
            | inRange ((1, 1), (n, m)) pos
            , (maze ! pos) `elem` ['.', 'G'] = True
            | otherwise                      = False
        f2 = (`S.notMember` s)
```

- コード汚いけど…
- 余再帰を使って無限リスト`queue`を作っている
    - 余再帰よく理解できてない
    - [なぜ Haskell ではキューが軽んじられているか？](https://kazu-yamamoto.hatenablog.jp/entry/20121107/1352259739)
- 既にその場所を訪れたかの判定に`Data.Set`を渡している
- 最後に無限リスト`queue`からゴール取り出して終了


キューが空になったら…っていうのがやりたいときは

- `go`の引数でキューの中身の数を管理して`0`になったら終了
    - `[]`でパターンマッチはできないことに注意
- 素直に`Data.Sequence`を使う
- 自分で`data Queue a = Queue [a] [a]`みたいにキューを作る
    - 左側から取り出して右側に追加する
    - 左側から取り出す時に空だったら右側を`reverse`して左側にセットする
