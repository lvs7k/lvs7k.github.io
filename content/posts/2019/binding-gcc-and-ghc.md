---
title: "GCCとHaskellのFFIについて学習メモ"
date: 2019-01-08T20:24:01+09:00
draft: false
categories: ["programming"]
tags: ["haskell", "c"]
---

## このページより読むべきリンク

上の南洋理工大学のホームページがマジでわかりやすい

- [GCC and Make - A Tutorial on how to compile, link and build C/C++ applications](http://www.ntu.edu.sg/home/ehchua/programming/cpp/gcc_make.html)
- [栄光のグラスゴーHaskellコンパイルシステム利用の手引き　バージョン7.8.2](http://www.kotha.net/ghcguide_ja/latest/)


## GCCのコンパイルプロセス

参考リンクを読め

1. Pre-processing
    - ヘッダのインクルード（`#include`）とマクロ（`#define`）の展開（`.i, .ii`）
2. Compilation
    - プリプロセスされたソースからアセンブリを出力（`.s`）
3. Assembly
    - アセンブリからオブジェクトコードを出力（`.o`）
4. Linker
    - オブジェクトコードとライブラリをリンクして実行ファイルを出力（`.exe`, `.a`, `.dll`）

- GCCはそれぞれのプロセスを行うプログラムを呼び出している
    - `cpp`
    - `cc1`
    - `as`
    - `ld`


## 静的ライブラリと動的ライブラリ

- 静的ライブラリ
    - 複数のオブジェクトコードを`ar`というコマンドでまとめたもの
- 動的ライブラリ
    - 複数のプログラムで共有して使用することができるライブラリ


## GCCで覚えるべきオプション

- `-o`
    - 出力ファイル名を指定
- `-v`
    - これつけてコンパイルすると知りたい情報が得られるかも
- `-l`
    - 静的ライブラリの`lib<ここ>.a`を指定
    - 動的ライブラリの`<ここ>.dll`を指定
- `-L`
    - ライブラリのあるディレクトリ
- `-I`
    - ヘッダファイルのあるディレクトリ
- `-shared`
    - `DLL`を作成する時に


## コンパイル済みのファイルを調べるのに使うコマンド

- `file`
- `nm`
- `ldd`


## Makefileの書き方

参考リンクを読め


----


## Haskell FFI

## RWH17章 PCREのコンパイル

- `pacman -S mingw64/mingw-w64-x86_64-pcre`
    - `msys`ではなく`mingw64`のをインストールする

- `rwh17.cabal`

    ```cabal
    executable rwh17
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
    include-dirs:        D:\msys64\mingw64\include
    extra-lib-dirs:      D:\msys64\mingw64\lib
    extra-libraries:     pcre
    ```

    - `package.yaml`と`stack.yaml`に設定追加でも同じ
    - `stack build --ghc-options="-ID:\msys64\mingw64\include -LD:\msys64\mingw64\lib -lpcre"`でも同じ

- `cabal`に`extra-libraries`を設定するとVSCodeのSimple GHCが動作しなくなる
    - `"ghcSimple.workspaceType": "bare-stack"`でとりあえず動く

- `stack exec -- hsc2hs src/Regex.hsc -v -ID:/msys64/mingw64/include`
    - 指定しないとGHCのincludeしか探しに行かないので自分で追加する必要あり
