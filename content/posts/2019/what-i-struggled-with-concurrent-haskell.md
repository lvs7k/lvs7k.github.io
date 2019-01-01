---
title: "Haskellのマルチスレッドプログラミングについてメモ"
date: 2019-01-01T22:09:19+09:00
draft: false
categories: ["programming"]
tags: ["haskell"]
---

## Haskellによる並列・並行プログラミング学習中

- 随時更新
- 多分相当レベル低いこと書いてる


## サブスレッドで発生した例外はメインスレッドへ伝播しない

```haskell
main :: IO ()
main = do
  e <- try $ do
    forkIO $ throwIO UserInterrupt
    return ()
  print (e :: Either SomeException ())
```

```
*Main> :main
<interactive>: user interrupt
Right ()
```

## `race_ getLine (return ())`は入力があるまで終了しない

- 入力を待たずに終了させるいい方法があったら教えてください

```haskell
main :: IO ()
main = do
  (race_ (putStrLn =<< getLine) (return ()))
  `finally`
  (putStrLn "end")
```

```
*Main> :main
hello
end
```
