---
title: "初めてのCMake"
date: 2019-01-14T11:30:24+09:00
draft: false
categories: ["programming"]
tags: ["c"]
---

初めてCMake使ったからメモ

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

[GitHubに上げました](https://github.com/lvs7k/cmake-template)
