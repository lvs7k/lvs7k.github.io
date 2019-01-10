---
title: "2019年1月 開発環境構築メモ(Windows7)"
date: 2019-01-10T22:58:53+09:00
draft: false
categories: ["programming"]
tags: ["haskell", "hugo"]
---

## ユーザ環境変数

```
SCOOP = D:\scoop
STACK_ROOT = D:\sr
```


## インストール

- [PortableGit](https://git-scm.com/download/win)
- [MSYS2](https://www.msys2.org/)
- [Scoop](https://scoop.sh/)
- [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
- [Visual Studio Code](https://code.visualstudio.com/)


## MSYS2

- `~/.bash_profile`

```bash
P1="/c/PortableGit/cmd"
P2="/d/stack/local/bin"
P3="/d/scoop/shims"
PATH="${PATH}:${P1}:${P2}:${P3}"

if [ -f "${HOME}/.bashrc" ] ; then
  source "${HOME}/.bashrc"
fi
```

- `~/.bashrc`

```bash
source ~/git-completion.bash
eval "$(stack --bash-completion-script stack)"
```

- `git`と`stack.exe`でTAB補完が効くようにする
- `stack.exe`でないと補完が効かない（`stack`ではダメ）
- [git-completion.bash](https://github.com/git/git/tree/master/contrib/completion)


## Stack

- `$STACK_ROOT/config.yaml`

```yaml
templates:
  params:
    author-name: lvs7k
    author-email: lvs7k@example.com
    category: Your Projects Category
    copyright: 'Copyright (c) 2019 lvs7k'
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
    "ghcSimple.bareStartupCommands": [],
    "ghcSimple.workspaceType": "bare-stack",
    "[haskell]": {
        "editor.tabSize": 2,
        "editor.detectIndentation": false
    },
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
// ターミナルの切り替え
{
  "key": "ctrl+shift+j",
  "command": "workbench.action.terminal.focusNext",
  "when": "terminalFocus"
},
{
  "key": "ctrl+shift+k",
  "command": "workbench.action.terminal.focusPrevious",
  "when": "terminalFocus"
}
]
```


## その他

- `pacman -Syu`
- `pacman -S base-devel`
- `pacman -S mingw-w64-x86_64-toolchain`
- `stack setup`
- `scoop install hugo`


## GHCiでctrl-cが動作しない

- `stack exec -- ghcii.sh`は`invalid argument`エラーで使えない
- `pacman -S winpty`
- `winpty stack ghci`


## VSCode拡張のSimple GHCが動作しない

- ファイルパスにスペースが含まれていると動作しないバグあり
- [Space in file path causes breakage](https://github.com/dramforever/vscode-ghc-simple/issues/14)


## 参考リンク

- [FAQ | MinGW](http://www.mingw.org/wiki/FAQ)
- [Home · msys2/msys2 Wiki](https://github.com/msys2/msys2/wiki)
    - [About terminals, consoles and shells](https://github.com/msys2/msys2/wiki/Terminals)
- [pacman - ArchWiki](https://wiki.archlinux.jp/index.php/Pacman)