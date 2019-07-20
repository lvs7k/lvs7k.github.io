---
title: "2019年7月 開発環境構築メモ(Windows7)"
date: 2019-07-20T18:53:00+09:00
draft: false
categories: ["programming"]
tags: ["haskell", "hugo"]
---

しばらくプログラミングをしていなかったので環境を見直してみる。どうせ大してやらないので最小限やることだけ書く。


## ユーザ環境変数

```
SCOOP = D:\scoop
MSYS2_PATH_TYPE = inherit
```


## インストール

- [Scoop](https://scoop.sh/)
- [MSYS2](https://www.msys2.org/)
- [Visual Studio Code](https://code.visualstudio.com/)

MSYS2とVisual Studio CodeもScoopでインストールしても良いかもしれない。


## インストール後に

- `scoop install git`
- `scoop install haskell`
- `scoop install hugo`


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
}
```

- `keybindings.json`

```json
// 既定値を上書きするには、このファイル内にキー バインドを挿入します
[
// ctrl+enterで行末へ移動
{
  "key": "ctrl+enter",
  "command": "cursorEnd",
  "when": "editorTextFocus && !editorReadonly"
},
]
```


## 参考リンク

- [FAQ | MinGW](http://www.mingw.org/wiki/FAQ)
- [Home · msys2/msys2 Wiki](https://github.com/msys2/msys2/wiki)
    - [About terminals, consoles and shells](https://github.com/msys2/msys2/wiki/Terminals)
- [pacman - ArchWiki](https://wiki.archlinux.jp/index.php/Pacman)
