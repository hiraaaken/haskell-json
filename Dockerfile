FROM haskell:9.4

WORKDIR /app

# 必要なパッケージをインストール
RUN cabal update

# ソースコードをコピー
COPY . .

# デフォルトコマンド
CMD ["ghci"]
