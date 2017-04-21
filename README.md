# xytoot

Mastodon client for xyzzy


## インストール

1. https://github.com/youz/xytoot/releases/ からソースをDLして展開する。あるいはリポジトリをクローンする。

2. `*load-path*`にあるディレクトリ(~/site-lisp等)にxytoot.l, xytoot.lcをコピーする。
   あるいは`*load-path*`に取得したフォルダのパスを追加する。
   (リポジトリをクローンして取得した場合はxytoot.lcが存在しないので、コピー後にM-x byte-compile-fileをして生成しておく。)

3. `.xyzzy`に`(require "xytoot")`と追記するなどして読み込む。

4. `M-x xytoot-add-user`を実行して次の手順でログインする。
    1. Mastodonサーバー名を入力 (`mastodon.cloud`, `mstdn.jp`等)
    2. ウェブブラウザが起動して認証ページを開くのでログインする
    3. アプリ認証用のコード(ワンタイムパスワード)が表示されるのでコピーし、xyzzyに戻って入力する
    4. アプリ認証完了

認証が完了すると `M-x xytoot` で起動できるようになります。
マルチアカウント対応なので、複数のユーザーを使い分ける場合はデフォルトの
ユーザー設定を`.xyzzy`に書いておくと良いでしょう。

    ;;; 設定例
    (add-to-list *load-path* "C:/path/to/xytoot/site-lisp")
    (require "xytoot")
    (setq xytoot:*default-user* "yourusername@mastodon.example.com")

また、xytootの設定保存用フォルダ `~/.xytoot` にconfig.l というファイルを置いておくと
xytoot起動時に読み込まれるので、このファイルにユーザー設定を書いておいても良いです。

[サンプル](https://github.com/youz/xytoot/blob/master/dotfolder_sample/config.l)


## キーマップ

### タイムラインバッファ
- j / k  -- 移動
- l, TAB -- 次のリンク (ユーザー名 or ハッシュタグ or URL) へ移動
- h      -- 前のリンクへ移動
- RET    -- リンクを開く
- R      -- リロード (新着tootの取得)
- J      -- ページ追加 (過去tootの取得)

- t      -- 投稿バッファを開く
- @      -- 返信用バッファを開く
- m      -- 投稿バッファを開く (カーソル下のユーザー名 or 投稿者の名前が入る)
- r b    -- Reblog
- f a    -- お気に入り on / off
- D      -- 投稿を削除
- f o    -- カーソル下のユーザーをフォローする
- u f    -- カーソル下のユーザーをアンフォローする

- H      -- ホームタイムラインへ移動
- U      -- ユーザー検索
- S      -- ハッシュタグ検索
- F      -- お気に入りを開く
- Q      -- バッファを閉じる

- C      -- tootのURLをクリップボードにコピー
- c c    -- カーソル下のURLをクリップボードにコピー
- T      -- tootのJSONデータを表示

### 投稿用バッファ

- C-c C-c  -- 投稿
- C-c C-k  -- 入力中の文章を破棄して投稿用バッファを閉じる


## todo

- 投稿時の`sensitive`パラメータ
- 投稿時の`spoiler`パラメータ
- Streaming API
- 

## 作者

Yousuke Ushiki (<citrus.yubeshi@gmail.com>)

[yubeshi@mstdn.jp](http://mstdn.jp/@yubeshi)


## Copyright

MIT License を適用しています。
