---
title: "Hugoでこのブログを作った時のメモ"
date: 2018-11-10T17:10:21+09:00
draft: false
categories: ["programming"]
tags: ["hugo"]
---


[Quick Start | Hugo](https://gohugo.io/getting-started/quick-start/)


## 環境

- Windows 7
- Chocolatey v0.10.11
- git 2.19.1
- hugo 0.49


## Hugoのインストール

1. Chocolateyをインストール

2. Chocolateyでgitをインストール
    - `choco install git`

3. ChocolateyでHugoをインストール
    - `choco install hugo -confirm`

4. GitHubでリポジトリ作成
    - Repository name : `<username>.github.io`
    - Initialize this repository with a READMEにチェックを入れた


## Hugoで静的サイト生成

1. サイト作成
    - `hugo new site <フォルダ名>`
    - `cd <フォルダ名>`
    - `git init`

2. テーマの追加
    - `git add submodule https://github.com/dplesca/purehugo.git themes/purehugo`

3. コンフィグ設定
    - `config.toml`を変更

        ```toml
        baseURL = "https://lvs7k.github.io/"
        languageCode = "ja-jp"
        title = "lvs7k's blog"
        theme = "purehugo"
        Paginate = 10
        disqusShortname = "lvs7k"
        publishDir = "docs"

        [params]
            twitterName = "lvs7k"
            githubName = "lvs7k"
            description = "Just a Memo"
            hideShareOptions = true
        ```

4. 記事の作成
    - `hugo new posts/2018/how-to-create-this-blog.md`
    - マークダウンの先頭(`front matter`と呼ばれている部分)の`draft`を`false`に変更
        - これをしないと生成されたサイトに記事が表示されない
    - 適当になんか書く

5. テーマをちょっといじる
    - `theme`フォルダ内の`sidebar.html`をトップの`layouts`フォルダへコピー
    - `theme`フォルダの`layouts`より`layouts`フォルダのほうが優先されるってこと？

6. サーバを立ち上げて確認
    - `hugo server`
    - ブラウザで`http://localhost:1313/`にアクセス
    - 想定通りテーマや記事が表示されているかどうか確認

7. サイト生成
    - `hugo`
    - docsフォルダにサイトが生成される


## GitHubへデプロイ

1. `source`ブランチを作成
    - `git checkout -b source`

2. ここまでの作業分をコミット
    - `git add .`
    - `git commit -m "Initial commit"`

3. `source`ブランチを`push`
    - `git push origin source`

4. `Default branch`を`source`に変更
    - リポジトリの`Settings` -> `Branches` -> `Default branch`を`source`に変更

5. `master`ブランチをリモートから削除
    - `git push -f --delete origin master`

6. `docs`フォルダをsubtreeとして(？)`master`という名前で`push`
    - `git subtree push --prefix docs/ origin master`

7. `<username>.github.io`にアクセスし確認


## 記事を追加するとき

1. デプロイ用のPowerShellスクリプト作成(`deploy.ps1`)

    ```powershell
    Write-Output "Deploying updates to GitHub..."

    # Build the project.
    hugo

    # Add changes to git.
    git add .

    # Commit changes.
    $msg = "Rebuilding site $(date)"

    if ($Args.Count -eq 1) {
        $msg = $Args[0]
    }

    git commit -m "$msg"

    # Push source and build repos.
    git push origin source
    git subtree push --prefix docs/ origin master
    ```

    - 使い方
        - `deploy.ps1 ["<commit message>"]`

2. 記事を書く
    - `hugo new posts/2018/kiji-wo-kaku.md`

3. デプロイ
    - `deploy.ps1 "initial commit"`


### どうしてsourceブランチを作ったりmasterブランチを削除したりしているか

- [GitHub PagesのUser Pagesでドキュメントルートを変更するにはmasterを殺す](https://qiita.com/kwappa/items/03ffdeb89039a7249619)より

> GitHub PagesにはUser & Organization PagesとProject Pagesの2種類がある。Project Pagesはgh-pagesというブランチか/docs以下をドキュメントルートにする「Source」という設定ができたのだが、User & Organization Pagesではmasterのルート直下しか選べない。

> 「Source」が設定できるようになる前はgh-pagesというブランチを用意するしかなかったのだが、これもルート直下がドキュメントルートになってしまう。masterの/docsをsubtreeとしてpushする、というテクニックが以前から使われていたので、それをmasterとして使ってみたというわけ。


## 参考資料

- [ブログのテーマ(とGitの管理方法)を変えた](https://myuon.github.io/posts/blog-simplicity/)
- [Hugo + GitHub Pagesでポートフォリオを作る](http://kohki.hatenablog.jp/entry/hugo-portfolio)
- [GitHub PagesのUser Pagesでドキュメントルートを変更するにはmasterを殺す](https://qiita.com/kwappa/items/03ffdeb89039a7249619)