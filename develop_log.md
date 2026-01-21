# JSON Parser Generator 技術解説

このドキュメントでは、`Main.hs` の Generator 部分で使用されている Haskell の技術について解説します。

## 目次

1. [Gen モナドとは](#gen-モナドとは)
2. [Functor と Applicative](#functor-と-applicative)
3. [QuickCheck コンビネータ](#quickcheck-コンビネータ)
4. [ポイントフリースタイル](#ポイントフリースタイル)
5. [Arbitrary 型クラス](#arbitrary-型クラス)
6. [再帰的データ生成のテクニック](#再帰的データ生成のテクニック)

---

## Gen モナドとは

`Gen a` は「型 `a` の値をランダムに生成する計算」を表す型です。QuickCheck ライブラリの中核となる型で、モナドとして扱えます。

```haskell
-- Gen は「ランダムな値を生成するレシピ」と考えられる
jNullGen :: Gen JValue
jNullGen = pure JNull  -- 常に JNull を生成する
```

`pure` はモナドの文脈で「何もしないで値をそのまま返す」という意味です。`pure JNull` は「常に JNull を生成する Generator」になります。

---

## Functor と Applicative

### `<$>` (fmap)

`<$>` は `fmap` の中置演算子版で、Generator の結果に関数を適用します。

```haskell
jBoolGen :: Gen JValue
jBoolGen = JBool <$> arbitrary
--         ↑ 関数   ↑ Gen Bool を生成
-- 結果: Gen JValue
```

これは以下と同じ意味です：

```haskell
jBoolGen = fmap JBool arbitrary
-- または
jBoolGen = do
  b <- arbitrary  -- Bool をランダム生成
  return (JBool b)
```

### `<*>` (Applicative apply)

`<*>` は「関数が入った Generator」と「値が入った Generator」を組み合わせます。

```haskell
jNumberGen :: Gen JValue
jNumberGen = JNumber <$> arbitrary <*> listOf (choose (0, 9)) <*> arbitrary
--           ↑                        ↑                          ↑
--           Integer -> [Int] -> Integer -> JValue
--           Gen Integer         Gen [Int]            Gen Integer
```

#### 段階的な解説

```haskell
-- JNumber は3つの引数を取るコンストラクタ
JNumber :: Integer -> [Int] -> Integer -> JValue

-- Step 1: JNumber <$> arbitrary
-- arbitrary :: Gen Integer
-- JNumber <$> arbitrary :: Gen ([Int] -> Integer -> JValue)
-- 「部分適用された関数」が Gen の中に入る

-- Step 2: ... <*> listOf (choose (0, 9))
-- listOf (choose (0, 9)) :: Gen [Int]
-- 結果: Gen (Integer -> JValue)

-- Step 3: ... <*> arbitrary
-- arbitrary :: Gen Integer
-- 結果: Gen JValue
```

Applicative スタイルを使うと、複数のランダム値を組み合わせて新しい値を作る処理を簡潔に書けます。

---

## QuickCheck コンビネータ

### `arbitrary`

`Arbitrary` 型クラスのメソッドで、その型のランダムな値を生成します。

```haskell
arbitrary :: Arbitrary a => Gen a

-- 例
arbitrary :: Gen Bool     -- True または False
arbitrary :: Gen Integer  -- ランダムな整数
```

### `oneof`

リストから Generator をランダムに1つ選んで実行します。

```haskell
oneof :: [Gen a] -> Gen a

-- 例: スカラー型のいずれかを生成
oneof [jNullGen, jBoolGen, jStringGen, jNumberGen]
```

### `frequency`

重み付きで Generator を選択します。

```haskell
frequency :: [(Int, Gen a)] -> Gen a

-- 例: n < 5 のとき
frequency [(4, oneof scalarGens), (1, oneof compositeGens)]
-- スカラー型が4倍の確率で選ばれる
```

これにより、浅い構造ではスカラー値が多く、深い構造では複合型が多くなるよう制御しています。

### `listOf`

Generator を使ってランダムな長さのリストを生成します。

```haskell
listOf :: Gen a -> Gen [a]

-- 例
listOf (choose (0, 9))  -- [0..9] の数字からなるランダムなリスト
```

### `vectorOf`

指定した長さのリストを生成します。

```haskell
vectorOf :: Int -> Gen a -> Gen [a]

-- 例: 16進数4文字
vectorOf 4 (elements hexDigitLetters)  -- "a3F2" のような文字列
```

### `elements`

リストからランダムに1つ選びます。

```haskell
elements :: [a] -> Gen a

-- 例
elements [' ', '\n', '\r', '\t']  -- 空白文字のいずれか
```

### `choose`

範囲内からランダムに値を選びます。

```haskell
choose :: Random a => (a, a) -> Gen a

-- 例
choose (0, 9)  -- 0〜9 のいずれかの数字
```

### `scale`

Generator のサイズパラメータを変換します。

```haskell
scale :: (Int -> Int) -> Gen a -> Gen a

-- 例
scale (`div` 2) (listOf gen)  -- サイズを半分にして生成
```

### `sized`

現在のサイズパラメータを使って Generator を作ります。

```haskell
sized :: (Int -> Gen a) -> Gen a

-- 例
instance Arbitrary JValue where
  arbitrary = sized jValueGen
  -- サイズパラメータを jValueGen に渡す
```

---

## ポイントフリースタイル

ポイントフリースタイルは、関数の引数を明示的に書かずに関数合成で表現するスタイルです。

### 例1: `jArrayGen`

```haskell
jArrayGen :: Int -> Gen JValue
jArrayGen = fmap JArray . scale (`div` 2) . listOf . jValueGen . (`div` 2)
```

これを展開すると：

```haskell
jArrayGen n = fmap JArray (scale (`div` 2) (listOf (jValueGen (n `div` 2))))
-- または
jArrayGen n = do
  let halfN = n `div` 2
  elements <- scale (`div` 2) $ listOf $ jValueGen halfN
  return $ JArray elements
```

#### 関数合成の流れ

```
n (Int)
  ↓ (`div` 2)         -- n を 2 で割る
n/2 (Int)
  ↓ jValueGen         -- JValue の Generator を作る
Gen JValue
  ↓ listOf            -- リストの Generator に変換
Gen [JValue]
  ↓ scale (`div` 2)   -- 生成サイズを半分に
Gen [JValue]
  ↓ fmap JArray       -- リストを JArray で包む
Gen JValue
```

### セクション記法

```haskell
(`div` 2)  -- \x -> x `div` 2 と同じ
```

中置演算子の片側を埋めた部分適用です。

---

## Arbitrary 型クラス

`Arbitrary` はランダムなテストデータを生成するための型クラスです。

```haskell
class Arbitrary a where
  arbitrary :: Gen a           -- ランダム値の生成
  shrink    :: a -> [a]        -- 失敗時の値の縮小
```

### JValue の Arbitrary インスタンス

```haskell
instance Arbitrary JValue where
  arbitrary = sized jValueGen  -- サイズパラメータを使って生成
  shrink    = genericShrink    -- Generic を使った自動縮小
```

#### `sized` の役割

QuickCheck はテストの進行に応じてサイズパラメータを増やしていきます。`sized` を使うことで：

- 最初は小さな値（`n` が小さい）
- 徐々に大きな値（`n` が大きい）

を生成するようになります。

#### `genericShrink` の役割

`GHC.Generics` を使って、失敗したテストケースを自動的に最小化します。`deriving (Generic)` と組み合わせて使います。

```haskell
data JValue = ...
            deriving (Eq, Generic)  -- Generic の導出が必要
```

---

## 再帰的データ生成のテクニック

JSON は再帰的なデータ構造（配列の中に配列、オブジェクトの中にオブジェクト）を持ちます。無限に深い構造を生成しないための工夫が必要です。

### サイズパラメータによる制御

```haskell
jValueGen :: Int -> Gen JValue
jValueGen n = if n < 5
  then frequency [(4, oneof scalarGens), (1, oneof (compositeGens n))]
  else frequency [(1, oneof scalarGens), (4, oneof (compositeGens n))]
```

- `n < 5` のとき：スカラー値（終端）を優先（4:1）
- `n >= 5` のとき：複合型を優先（1:4）

### 再帰時のサイズ縮小

```haskell
jArrayGen :: Int -> Gen JValue
jArrayGen = fmap JArray . scale (`div` 2) . listOf . jValueGen . (`div` 2)
--                        ↑ リスト長を縮小       ↑ 再帰の深さを縮小
```

再帰のたびに `n` を半分にすることで、構造が深くなるにつれて終端しやすくなります。

---

## stringify 関数の解説

`stringify` は `JValue` を JSON 文字列に変換しますが、ホワイトスペースをランダムに挿入します。

```haskell
stringify :: JValue -> Gen String
stringify = pad . go
```

### ローカル関数の解説

```haskell
surround l r j = l ++ j ++ r        -- 文字列を左右で挟む
pad gen = surround <$> jsonWhitespaceGen <*> jsonWhitespaceGen <*> gen
--        ランダムな空白で挟む
commaSeparated = pad . pure . intercalate ","
--              カンマ区切りにして空白で挟む
```

### JArray の処理

```haskell
JArray elements ->
  mapM (pad . stringify) elements           -- 各要素を stringify して pad
    >>= fmap (surround "[" "]") . commaSeparated  -- カンマ区切りで [] で囲む
```

`mapM` は `[Gen String]` ではなく `Gen [String]` を返すモナディックな map です：

```haskell
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-- ここでは
mapM (pad . stringify) elements :: Gen [String]
```

---

## まとめ

| 技術 | 用途 |
|------|------|
| `Gen` モナド | ランダム値生成の抽象化 |
| `<$>`, `<*>` | 複数の Generator の組み合わせ |
| `frequency`, `oneof` | 確率的な選択 |
| `scale`, `sized` | 生成サイズの制御 |
| ポイントフリー | 関数合成による簡潔な記述 |
| `Arbitrary` | テストデータ生成のインターフェース |
| サイズ縮小 | 再帰的構造の有限化 |

これらの技術を組み合わせることで、複雑な JSON 構造をランダムに生成し、パーサーのプロパティベーステストを可能にしています。
