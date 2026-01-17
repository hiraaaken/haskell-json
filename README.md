# haskell-json

Haskell の学習・開発環境

## セットアップ

### 必要なもの

- Docker
- Docker Compose

### コンテナのビルド

```bash
docker compose build
```

## 使い方

### GHCi（対話環境）を起動

```bash
docker compose run --rm haskell
```

GHCi 内でファイルを読み込む場合：

```haskell
:load Main.hs
main
```

### Haskell ファイルを実行

```bash
docker compose run --rm haskell runhaskell Main.hs
```

### コンパイルして実行（ワンライナー）

```bash
docker compose run --rm haskell sh -c "ghc Main.hs -o main && ./main"
```

### コンテナ内でコンパイルして実行

コンテナ内でシェルを起動：

```bash
docker compose run --rm haskell bash
```

コンテナ内でコンパイル：

```bash
ghc Main.hs -o main
```

実行：

```bash
./main
```
