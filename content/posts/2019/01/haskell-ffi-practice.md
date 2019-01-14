---
title: "HaskellでFFIを書く準備"
date: 2019-01-13T15:19:35+09:00
draft: false
categories: ["programming"]
tags: ["haskell"]
---

以下の2つのパターンについてFFIをやってみた。

1. ライブラリで自作のCの関数を使う
2. 外部のライブラリに対するバインディングを書く


## 1. ライブラリで自作のCの関数を使う

[GitHubに上げました](https://github.com/lvs7k/haskell-ffi-example)

- フォルダ構成

```
app
  - Main.hs
cbits
  - person.c
include
  - person.h
src
  - Person.hsc
```

- `package.yaml`

```yaml
extra-source-files:
- README.md
- ChangeLog.md
- include/**
- cbits/**

library:
  source-dirs: src
  include-dirs:
  - include
  c-sources:
  - cbits/*.c
  build-tools:
  - hsc2hs:hsc2hs
```

これだけで`stack build`して`stack run`できる。


## 2. 外部のライブラリに対するバインディングを書く

例えばRWH17章のように`PCRE`のバインディングを書くなら

- 準備

```
pacman -S mingw64/mingw-w64-x86_64-pcre
```

- フォルダ構成

```
app
  - Main.hs
include
  - pcre.h    -- mingw64/includeからコピー
src
  - Pcre.hsc
```

- `package.yaml`

```yaml
extra-source-files:
- README.md
- ChangeLog.md
- include/**

library:
  source-dirs: src
  extra-libraries:
  - pcre
  include-dirs:
  - include
  build-tools:
  - hsc2hs:hsc2hs
```

- `stack.yaml`

```yaml
# Extra directories used by stack for building
extra-include-dirs:
- D:\msys64\mingw64\include
extra-lib-dirs:
- D:\msys64\mingw64\lib
```


## メモ

- `extra-libraries`を設定するとVSCodeのSimple GHCが動作しなくなる
    - `"ghcSimple.workspaceType": "bare-stack"`でとりあえず動く

- `hsc2hs`を使って手動で変換したいとき
    - `stack exec -- hsc2hs src/Pcre.hsc -I./include`
    - `-I`で指定しないとGHCのincludeしか探しに行かない
