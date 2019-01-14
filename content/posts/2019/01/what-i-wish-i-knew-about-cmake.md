---
title: "初めてのCMake"
date: 2019-01-14T11:30:24+09:00
draft: false
categories: ["programming"]
tags: ["c"]
---

初めてCMake使ったからメモ

## Hello, world!

- フォルダ構成

```
include
  - hello.h
src
  - hello.c
CMakeLists.txt
```

- `hello.h`

```c
void hello();
```

- `hello.c`

```c
#include <stdio.h>
#include "hello.h"

int main()
{
    hello();
    return 0;
}

void hello()
{
    printf("Hello, world!\n");
}
```

- `CMakeLists.txt`

```
cmake_minimum_required(VERSION 2.8)
project (hello_project)
include_directories(${CMAKE_SOURCE_DIR}/include)
add_executable(hello src/hello.c)
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

*TODO: 具体例を書く*

- `cmake --help-module-list | grep ^Find`で出てくるライブラリ
    - `find_package`を使う
- それ以外
    - 正しくは`.cmake`ファイルを作成して行う（参考リンク参照）
    - 簡単なものなら`find_library`を使う


## 参考

- [How To Find Libraries · Wiki · CMake / Community · GitLab](https://gitlab.kitware.com/cmake/community/wikis/doc/tutorials/How-To-Find-Libraries)