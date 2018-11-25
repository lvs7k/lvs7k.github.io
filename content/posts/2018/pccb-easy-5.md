---
title: "Haskellで蟻本（初級編） - グラフ"
date: 2018-11-23T17:19:12+09:00
draft: false
categories: ["competitive"]
tags: ["haskell"]
---

※蟻本の入力例でしかテストしていません

*TODO: コードをもっと綺麗にする*


## 入力の形式について

標準入力は下記の形式で与えられるものとする。

- `n: 頂点数`
- `m: 辺の数`
- `c: 重み`

```
n m
f1 t1
f2 t2
...
fn tn
```

```
n m
f1 t1 c1
f2 t2 c2
...
fn tn cn
```


## グラフの表現

隣接行列も隣接リストも`accumArray`を使えば良い。

### 隣接行列

```haskell
{-# LANGUAGE BangPatterns, FlexibleContexts #-}

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array
import Data.STRef
import Data.List

-- 隣接行列
buildGMatrix :: (Int, Int) -> [(Int, Int)] -> Array (Int, Int) Int
buildGMatrix (i, j) es = accumArray (flip const) 0 ((i, i), (j, j)) (zip es (repeat 1))
```

### 隣接リスト

```haskell
-- 隣接リスト
buildGList :: (Int, Int) -> [(Int, a)] -> Array Int [a]
buildGList (i, j) es = accumArray (flip (:)) [] (i, j) es
```

- `containers`の`Data.Graph.buildG`と同じ

----

## グラフの探索

```haskell
-- 二部グラフ判定
q1 :: Int -> Int -> [(Int, Int)] -> Bool
q1 n m es = runST $ do
    color <- newArray (0, n - 1) 0 :: ST s (STUArray s Int Int)
    bs <- forM [0 .. n - 1] $ \i -> do
        ci <- readArray color i
        if ci == 0
            then go color 1 i
            else return True
    return (and bs)
  where
    g = buildGList (0, n - 1) es
    go color c i = do
        ci <- readArray color i
        if ci == 0
            then do
                writeArray color i c
                and <$> sequence (fmap (go color (-c)) (g ! i))
            else return (ci == c)
```

----

## 最短経路問題

### ベルマンフォード法

```haskell
-- ベルマン-フォード法
bellman :: Int -> Int -> Int -> [(Int, Int, Int)] -> Array Int Int
bellman n m start es = runSTArray $ do
    d <- newArray (0, n - 1) maxBound :: ST s (STArray s Int Int)
    writeArray d start 0
    go d
    return d
 where
    go d = do
        rupd <- newSTRef False
        forM_ es $ \(f, t, c) -> do
            df <- readArray d f
            dt <- readArray d t
            when (df /= maxBound && df + c < dt) $ do
                writeArray d t (df + c)
                writeSTRef rupd True
        readSTRef rupd >>= \upd -> when upd (go d)
```

- 用意するもの
    - 辺のリスト
    - 1次元配列
        - 初期化：始点のみ`0`, 残りは`INF`
        - `maxBound`に加算すると負になってしまうので注意
- ループ
    - 辺のリストに対してループ
    - 辺の始点と終点の距離を配列から取得し、終点の距離を更新
    - 1つでも更新できたら再帰呼び出し

### ダイクストラ法

```haskell
-- ダイクストラ法
dijkstra :: Int -> Int -> Int -> [(Int, Int, Int)] -> Array Int Int
dijkstra n m start es = runSTArray $ do
    d <- newArray (0, n - 1) maxBound :: ST s (STArray s Int Int)
    writeArray d start 0
    go d (push (0, start) Empty :: Skew (Int, Int))
    return d
  where
    g = buildGList (0, n - 1) [(a, (b, c)) | (a, b, c) <- es]
    go d que
        | Just ((c, f), q) <- pop que = do
            rq <- newSTRef q
            df <- readArray d f
            when (c <= df) $ do
                forM_ (g ! f) $ \(t, c) -> do
                    dt <- readArray d t
                    when (df + c < dt) $ do
                        writeArray d t (df + c)
                        modifySTRef' rq (\q -> push (df + c, t) q)
            go d =<< readSTRef rq
        | otherwise = return ()
```

- 用意するもの
    - グラフ（隣接リスト）
    - 優先度付きキュー
        - `(コスト, 頂点)`の並びでタプルにし、コストの小さい順で取り出せるようにする
        - 初期化：`(0, 始点)`
    - 1次元配列
        - 初期化：始点のみ`0`, 残りは`INF`
- ループ
    - キューが空になるまでループ
    - キューのコストと配列のコストを比較し配列のほうが既に小さいときはスキップ
    - グラフより隣接する頂点を取り出し、更新できるときは
        - 配列を更新
        - キューにも追加

### ワーシャル-フロイド法

```haskell
-- ワーシャル-フロイド法
warshall :: Int -> Int -> [(Int, Int, Int)] -> Array (Int, Int) Int
warshall n m es = runSTArray $ do
    d <- newArray ((0, 0), (n - 1, n - 1)) inf
    sequence_ [writeArray d (i, i) 0 | i <- [0 .. n - 1]]
    sequence_ [writeArray d (f, t) c | (f, t, c) <- es]
    forM_ [0 .. n - 1] $ \k -> do
        forM_ [0 .. n - 1] $ \i -> do
            forM_ [0 .. n - 1] $ \j -> do
                dij <- readArray d (i, j)
                dik <- readArray d (i, k)
                dkj <- readArray d (k, j)
                writeArray d (i, j) $ min dij (dik + dkj)
    return d
  where
    inf = 10 ^ 9 + 7
```

- 用意するもの
    - 辺のリスト
    - 2次元配列
        - 初期化
            - 始点と終点が同じ場合は`0`
            - 辺が存在する場合はそのコスト
            - それ以外は`INF`
- ループ
    - 3重ループ
    - 1番外側のループがその頂点を経由するかしないか
    - 2番目と3番目のループが始点と終点
    - 経由するかしないかで距離が短いほうで更新

### 経路復元（ダイクストラ法）

```haskell
-- 経路復元（ダイクストラ法）
dijkstraPath :: Int -> Int -> Int -> Int -> [(Int, Int, Int)] -> [Int]
dijkstraPath n m start end es = runST $ do
    d <- newArray (0, n - 1) maxBound :: ST s (STArray s Int Int)
    p <- newArray (0, n - 1) (-1) :: ST s (STArray s Int Int)
    writeArray d start 0
    go d p (push (0, start) Empty :: Skew (Int, Int))
    path [end] p
  where
    g = buildGList (0, n - 1) [(a, (b, c)) | (a, b, c) <- es]
    go d p que
        | Just ((c, f), q) <- pop que = do
            rq <- newSTRef q
            df <- readArray d f
            when (c <= df) $ do
                forM_ (g ! f) $ \(t, c) -> do
                    dt <- readArray d t
                    when (df + c < dt) $ do
                        writeArray d t (df + c)
                        writeArray p t f
                        modifySTRef' rq (\q -> push (df + c, t) q)
            go d p =<< readSTRef rq
        | otherwise = return ()
    path (x:xs) p
        | x == start = return (x:xs)
        | otherwise  = do
            pt <- readArray p x
            path (pt:x:xs) p
```

- 更新時に手前の頂点をメモっておいて最後に復元する

----

## 最小全域木

### プリム法

```haskell
-- プリム法
prim :: Int -> Int -> Int -> [(Int, Int, Int)] -> Int
prim n m start es = runST $ do
    used <- newArray (0, n - 1) False :: ST s (STArray s Int Bool)
    writeArray used start True
    let ss = fmap (\(t, c) -> (c, t)) (g ! start)
    go 0 used (foldr push Empty ss)
  where
    g = buildGList (0, n - 1) [(a, (b, c)) | (a, b, c) <- es]
    go !k used que
        | Just ((c, t), q) <- pop que = do
            usedt <- readArray used t
            if usedt
                then go k used q
                else do
                    let ss = fmap (\(t, c) -> (c, t)) (g ! t)
                    let q' = foldr push q ss
                    writeArray used t True
                    go (k + c) used q'
        | otherwise = return k
```

- 用意するもの
    - グラフ（隣接リスト）
    - 優先度付きキュー
        - 初期化：始点から伸びている辺を`(コスト, 頂点)`の並びで追加
    - 1次元配列
        - 既にその頂点が最小全域木に含まれているかをメモする配列
        - 初期化：始点のみ`True`, 残りは`False`
- ループ
    - キューが空になるまでループ
    - キューから取り出した辺の終点が既に最小全域木に含まれている場合はスキップ
    - そうでなければ採用した辺の終点から伸びている辺を取り出しキューに追加
    - 採用した辺の終点をメモし、再帰

### クラスカル法

```haskell
-- クラスカル法
kruskal :: Int -> Int -> [(Int, Int, Int)] -> Int
kruskal n m es = runST $ do
    uf <- newUF (0, n - 1)
    rc <- newSTRef 0
    forM_ es' $ \(f, t, c) -> do
        isSame <- same uf f t
        when (not isSame) $ do
            unite uf f t
            modifySTRef' rc (+ c)
    readSTRef rc
  where
    es' = sortBy (\(_, _, a) (_, _, b) -> a `compare` b) es
```

- 用意するもの
    - 辺のリスト
        - コストの小さい順にソート
    - Union-Find木
- ループ
    - コストの小さい辺から見ていく
    - 辺の始点と終点が既に同じグループならスキップ
    - そうでなければその辺を採用して始点と終点を同じグループにする

----

## 練習問題

初級とか言ってるけど難しすぎでは…？

```haskell
-- Roadblocks (POJ No.3255)
q2 :: Int -> Int -> [(Int, Int, Int)] -> Int
q2 n m es = runST $ do
    d1 <- newListArray (1, 4) (0 : repeat inf) :: ST s (STUArray s Int Int)
    d2 <- newListArray (1, 4) (0 : repeat inf) :: ST s (STUArray s Int Int)
    go d1 d2 (push (0, 1) Empty)
    readArray d2 n
  where
    inf = 10 ^ 9 + 7
    g = buildGList (1, n) [(f, (t, c)) | (f, t, c) <- es]
    go d1 d2 que
        | Just ((cost, v), que') <- pop que = do
            rque <- newSTRef que'
            d2v <- readArray d2 v
            when (cost <= d2v) $ do
                forM_ (g ! v) $ \(t, c) -> do
                    d1t <- readArray d1 t
                    d2t <- readArray d2 t
                    let d2c = cost + c
                    when (d2c < d1t) $ do
                        writeArray d1 t d2c
                        modifySTRef' rque $ \q -> push (d2c, t) q
                    when (d2c < d2t && d1t < d2c) $ do
                        writeArray d2 t d2c
                        modifySTRef' rque $ \q -> push (d2c, t) q
            go d1 d2 =<< readSTRef rque
        | otherwise = return ()
```

- ダイクストラ法で最短距離を求めつつ2番目の最短路も求める
- キューから取り出して最短距離は更新できないが2番目の最短路が更新できるなら更新


```haskell
-- Conscription (POJ No.3723)
q3 :: Int -> Int -> Int -> [(Int, Int, Int)] -> Int
q3 n m r es = runST $ do
    uf <- newUF (0, n + m - 1)
    k <- kruskal 0 uf es'
    return $ 10000 * (n + m) + k
  where
    es' = sortBy (\(_, _, c1) (_, _, c2) -> c1 `compare` c2)
        $ concat [[(f, n + t, -c), (n + t, f, -c)] | (f, t, c) <- es]
    kruskal !k uf []             = return k
    kruskal !k uf ((f, t, c):rs) = do
        isSame <- same uf f t
        if isSame
            then kruskal k uf rs
            else do
                unite uf f t
                kruskal (k + c) uf rs
```

- 辺のコストを負にして最小全域木を求める
- なぜこれで答えが求まるのかいまいちわかってない


```haskell
-- Layout (POJ No.3169)
q4 :: Int -> Int -> Int -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> Int
q4 n ml md as bs = runST $ do
    d <- newListArray (1, n) (0 : repeat maxBound) :: ST s (STUArray s Int Int)
    forM_ [1 .. n] $ \_ -> do
        forM_ as  $ \e -> update d e
        forM_ bs' $ \e -> update d e
        forM_ cs  $ \e -> update d e
    d1 <- readArray d 1
    dn <- readArray d n
    case () of
        _ | d1 < 0         -> return (-1)
          | dn == maxBound -> return (-2)
          | otherwise      -> return dn
  where
    bs' = [(t, f, -c) | (f, t, c) <- bs]
    cs  = [(i + 1, i, 0) | i <- [1 .. n - 1]]
    update d (f, t, c) = do
        df <- readArray d f
        when (df < maxBound) $ do
            dt <- readArray d t
            writeArray d t (min dt (df + c))
```

- 普通の最短路問題
    - `d(v) + w >= d(u)`
        - 頂点vから頂点uへコストwの辺が存在
- この問題
    - `d[i+1] + 0     >= d[i]`
        - 頂点i+1から頂点iへコスト0の辺が存在
    - `d[AL]  + DL    >= d[BL]`
        - 頂点ALから頂点BLへコストDLの辺が存在
    - `d[BD]  + (-DD) >= d[AD]`
        - 頂点BDから頂点ADへコスト-DDの辺が存在
- 負の辺が含まれるためベルマン-フォード法で最短経路を求める
- 負の閉路が存在する場合
    - 例）1と3は仲良しで距離10以内、2と3は仲悪くて距離11以上離す
- いくらでも離れることが可能な場合
    - 例）Nは誰とも仲良くない
