---
title: "2018年12月 開発環境構築メモ(Windows7)"
date: 2018-12-23T12:50:53+09:00
draft: false
categories: ["programming"]
tags: ["haskell", "hugo"]
---

## ユーザ環境変数

```
PATH = C:\PortableGit\cmd;D:\stack\local\bin;D:\scoop\shims;
MSYS2_PATH_TYPE = inherit
SCOOP = D:\scoop
STACK_ROOT = D:\sr
```


## インストール

- [PortableGit](https://git-scm.com/download/win)
- [MSYS2](https://www.msys2.org/)
- [Scoop](https://scoop.sh/)
- [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
- [Visual Studio Code](https://code.visualstudio.com/)


## Stack

- `$STACK_ROOT/config.yaml`

```yaml
# This file contains default non-project-specific settings for 'stack', used
# in all projects.  For more information about stack's configuration, see
# http://docs.haskellstack.org/en/stable/yaml_configuration/

# The following parameters are used by "stack new" to automatically fill fields
# in the cabal config. We recommend uncommenting them and filling them out if
# you intend to use 'stack new'.
# See https://docs.haskellstack.org/en/stable/yaml_configuration/#templates
templates:
  params:
    author-name: lvs7k
    author-email: k.takano.7.000@gmail.com
    category: Your Projects Category
    copyright: 'Copyright (c) 2018 lvs7k'
    github-username: lvs7k

skip-msys: true
local-bin-path: D:\stack\local\bin
local-programs-path: D:\stack\
```


## Visual Studio Code

- `settings.json`

```json
// 既定の設定を上書きするには、このファイル内に設定を挿入します
{
    /* editor */
    "editor.fontSize": 20,
    "editor.rulers": [80],
    "editor.wordWrap": "off",
    "editor.multiCursorModifier": "ctrlCmd",
    "editor.acceptSuggestionOnEnter": "off",
    "files.trimTrailingWhitespace": true,
    "workbench.editor.enablePreview": false,
    "workbench.startupEditor": "none",
    "workbench.colorTheme": "Abyss",
    "extensions.autoUpdate": false,
    "zenMode.hideStatusBar": false,

    /* shell */
    "terminal.integrated.shell.windows": "D:\\msys64\\msys2_shell.cmd",
    "terminal.integrated.shellArgs.windows": [
        "-mingw64", "-defterm", "-no-start", "-here", "-full-path"
    ],
    "terminal.integrated.setLocaleVariables": true,

    /* vim */
    "vim.hlsearch": true,
    "vim.handleKeys": {
        "<C-d>": false,
        "<C-k>": false,
    },
    "vim.insertModeKeyBindings": [
        {
            "before": ["f", "d"],
            "after" : ["<Esc>"]
        }
    ],

    /* Haskell */
    "[haskell]": {
        "editor.tabSize": 2,
        "editor.detectIndentation": false
    },
}
```


## その他

- `stack setup`
- `scoop install hugo`
