---
title: "初めてのCMake"
date: 2019-01-14T11:30:24+09:00
draft: false
categories: ["programming"]
tags: ["c"]
---

初めてCMake使ったからメモ

## これより読むべき公式より分かりやすいチュートリアル

- [CMake Tutorial – Onur Dündar – Medium](https://medium.com/@onur.dundar1/cmake-tutorial-585dd180109b)

## 参考リンク

- [cmake-commands(7)](https://cmake.org/cmake/help/git-master/manual/cmake-commands.7.html)
- [CMakeを使ってみた](https://www.wagavulin.jp/entry/2011/11/27/222636)
- [How To Find Libraries](https://gitlab.kitware.com/cmake/community/wikis/doc/tutorials/How-To-Find-Libraries)
- [SIGIL - CMakeLists.txt](https://gitlab.com/geoff-nagy/sigil/blob/master/CMakeLists.txt)
- [kigster/cmake-project-template](https://github.com/kigster/cmake-project-template)


## Hello, world!

- フォルダ構成

```
hello.h
hello.c
CMakeLists.txt
```

- `CMakeLists.txt`

```
cmake_minimum_required(VERSION 3.0)
project (hello)
include_directories(${PROJECT_SOURCE_DIR})
add_executable(${PROJECT_NAME} hello.c)
```


## ビルド

```
mkdir build
cd build
cmake .. -G "MSYS Makefiles"
make VERBOSE=1
./hello.exe
```

```
Hello, world!
```


## ライブラリのリンク

- CMake付属のモジュールが存在する場合
    - `cmake --help-module-list`でモジュール一覧を表示
    - `find_package(OpenGL REQUIRED)`のように使う
- 存在しない場合
    - `find_library (<VAR> NAMES name1 [name2 ...] PATH [path1 path2 ...])`
    - 詳しくは[How To Find Libraries](https://gitlab.kitware.com/cmake/community/wikis/doc/tutorials/How-To-Find-Libraries)


----

## もう少し複雑な例

Haskell stackのnew-templateっぽいのを作ってみた

[My C project template](https://github.com/lvs7k/cmake-template-c)


## libfoo.dll.aって何？

- よくわからんが参考になりそうなStackOverflowの回答を見つけた
- [Building a shared library using gcc on Linux and MinGW on Windows - Stack Overflow](https://stackoverflow.com/questions/17601949/building-a-shared-library-using-gcc-on-linux-and-mingw-on-windows)

> On Windows, you need to create an import library for the DLL. An import library looks like a static library, in that it defines all of the needed symbols, but it doesn't have the actual function implementations, it just has stubs. The import library will resolve the "undefined reference" errors while avoiding static linking.

> To create an import library with MinGW, follow the instructions here. The key is that when building the DLL, you must pass the option -Wl,--out-implib,libexample_dll.a to the linker to generate the import library libexample_dll.a.

> Then, when you compile your main executable, you use the -lexample_dll option (along with -L.) to link against the import library.
