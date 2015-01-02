Haskellで作る音ゲー(翻訳: @stefafafan)
====
Fumiaki Kinoshita (IIJ-II) fumiexcel@gmail.com

イントロダクション
----
リズムゲームは日本で人気なゲームジャンルの1つです。__1998年にDance Dance Revolution (DDR)__ がコナミから発表されてジャンルの中で最も成功している。他に__太鼓の達人__も幅広い年代の人に愛されています。今日も次々と色んな種類のリズムゲームが発表されています。

しかし、この類のゲームのチュートリアルは少なく、あったとしても古すぎたりWindowsでしか動作しなかったりします。

このチュートリアルは苦痛無くリズムゲームを作れるようにと書きました。驚く無かれ、Haskellを使えばできます。

このチュートリアルは3つのパートに分かれています：
* パートIはパートIIとIIIで必要となってくる環境構築について。
* パートIIではシンプルなリズムゲームを作ります。Callエンジンを利用します。
* パートIIIはパートIIを支える技術的な背景（グラフィック、オーディオ等）を紹介します。

このチュートリアルによりゲームを作りたいという意欲が出たら嬉しいです。

パート I： 準備
----
まずはGHCをインストールしなければなりません。[Haskell Platform](https://www.haskell.org/platform/) でインストールするのが楽です。

UnixかMacでは `libportaudio19` をインストールしてください。

__注意：現時点で Call はMac OS Xにてビットマップをちゃんと描画してくれません。原因を分かる人が居れば教えて下さい。__

```
$ sudo <your-package-manager> install libportaudio19
```

このチュートリアルで使われるソースコードは `rhythm-game-tutorial` パッケージにあります。以下のコマンドでダウンロードとセットアップが出来ます：

```
$ cabal unpack rhythm-game-tutorial
$ cd rhythm-game-tutorial-<version>
$ cabal install --only-dependencies
% cabal configure
$ cabal build
```

`cabal install --only-dependencies` で様々なパッケージがインストールされます。中でも `objective` と `call` は重要なパッケージです。

* `objective` はステートフルなオブジェクトの抽象化をしてくれます。必要ではないが状態を扱うときの苦痛を和らいでくれます。
* `call` はクロスプラットフォームなマルチメディアライブラリです。軽量でシンプルでありながら、ゲームで使う様々な媒体（2D/3Dグラフィックス、オーディオ、キーボード・マウス・ゲームパッドからの入力等）に対応しています。
* `binding-portaudio` は低水準なオーディオのAPIです。

### Windowsにて

`bindings-portaudio` はインストールを楽にするため、ビルトインのソースを含んでいます。残念ながらGHCのバグにより時折不安定です。Windows x64の場合32ビット版を使ったほうが安定しているのでこういった問題は回避できます。

```shell
$ cabal install bindings-portaudio -fBundle -fWASAPI
```

よくわからないエラーを投げてきた場合は私に報告してください。

パート II: ゲームを作る
-------------------------------------------------

> さあ始まるドン！ -- 和田どん、 「太鼓の達人」

シンプルなゲームを思い浮かべてください：画面下に丸があって、他の丸が上から迫ってきます。ちょうど重なったタイミングでスペースキーを押すゲームです。

![tutorial-passive](images/tutorial-passive-screenshot.png)

どのようにして実装すればいいでしょうか？プログラムの構造は以下の要素から成り立ちます：

* __音楽__: ゲーム中に音楽が流れています。
* __グラフィックス__: 時間によってグラフィックスが変わります。
* __インタラクション__: プレイヤーがスペースキーを押した時にスコアが更新されます。

順に説明していきます。

### 音楽を再生する

グルーヴは大事です。音楽を再生しましょう。最初のゲームです(`src/music-only.hs`)：

```haskell
main = runSystemDefault $ do
  music <- prepareMusic "assets/Monoidal Purity.wav"
  playMusic music
  stand
```

実行しましょう：

```shell
$ dist/build/music-only/music-only
```

音楽が聴こえますか？音楽をロードするのに少々時間かかります。

コードを見てみましょう。以下の関数が Call エンジンによる定義されています。

```haskell
runSystemDefault :: (forall s. System s a) -> IO a
stand :: System s ()
```

Call では`System s` モナドにアクションが実行されます。`runSystemDefault` が `System s` を `IO` へと変換します。`stand` は何もしませんがプログラムの終了を止めます。

`prepareMusic` と `playMusic` のシグネチャは以下の通りです：

```haskell
type Music s = Inst (System s) (StateT Deck (System s)) (System s)

prepareMusic :: FilePath -> System s Music
playMusic :: Music -> System s ()
```

これらの関数は後ほど定義します。

### 画像の描画

ゲームのグラフィカルな部分を作っていきましょう。

```haskell
main = runSystemDefault $ do
  allTimings <- liftIO $ (!!0) <$> parseTimings (60/140*4) <$> readFile "assets/Monoidal-Purity.txt"
  linkPicture $ \_ -> renderLane allTimings <$> getTime
  stand
```

`linkPicture :: (Time -> System s Picture) -> System ()` がCallで定義されている唯一の何かを描画するための関数です。`linkPicture f` が繰り返し `f` を呼びその結果をウィンドウに描画します。`f` の引数はフレーム間の時間ですが普通は考えなくてよいです。

ゲームシステムの仕様のため、タイミング等を設定しないといけません。ここでただの数字の羅列よりも読みやすいタイミングの表記法を紹介します。

この表記法はいくつかのパケットによって成り立っていて、複数の小節を表しています。一つのパケットごとに複数の列を含んでいます。小節は列の長さにより分割されます。'.' は音符、'-' は休符です。

    ----.-----------
    .-----------.---
    --------.-------

パーサーの実装は単純です。

```haskell
parseTimings :: String -> [Set Time]
```

タイミングと丸の"寿命"があれば現在の時刻から丸の位置を計算できます。

```haskell
phases :: Set Time -- ^ タイミング
    -> Time -- ^ 寿命
    -> Time -- ^ 現在時刻
    -> [Float] -- ^ フェーズ
phases s len t = map ((/len) . subtract t) -- [0, 1]の範囲に変換
  $ Set.toList
  $ fst $ Set.split (t + len) s -- リミットより前
```

丸を描画する関数を作る。`Picture` はモノイドなので `foldMap` か `mconcat` を使って画像を組み合わせることができます。`translate (V2 x y)` を使って画像を (x, y) の座標へシフトさせます。`bitmap b` を使って `Bitmap` を `Picture` に変換します。

`unsafePerformIO` の型は `IO a -> a` であって見た感じとても見慣れない感じでしょう。`unsafePerformIO` の使用は `getArgs` や `readBitmap` のような__コンスタント__な操作にのみ限定されるべきです。

```haskell
circle_png :: Bitmap
circle_png = unsafePerformIO $ readBitmap "assets/circle.png"

circles :: [Float] -> Picture
circles = foldMap (\p -> V2 320 ((1 - p) * 480) `translate` bitmap circle_png)
```

`renderLane` は `phases` の結果を `circles` に渡します。 `color` で画像の色を指定します。

```haskell
renderLane :: Set Time -> Time -> Picture
renderLane ts t = mconcat [color blue $ circles (phases ts 1 t)
    , V2 320 480 `translate` color black (bitmap circle_png) -- 基準
    ]
```

現時点での `main` はこんな感じです。

```haskell
main = runSystemDefault $ do
  music <- prepareMusic "assets/Monoidal-Purity.wav"
  allTimings <- fmap (!!0) $ liftIO $ loadTimings "assets/Monoidal-Purity.txt"
  linkPicture $ \_ -> renderLane allTimings <$> getTime
  playMusic music
  stand
```

このプログラムにはまだ重要な問題点があります。なんらかの問題でプログラムがつまずくと画像と音楽が__ズレる__可能性があります。実際の時間ではなく音楽の時間を元にタイミングをとらないといけません。

### コンポーネント: prepareMusic

リズムゲームにおいて音楽は欠かせません。

```haskell
type Music s = InstOf (System s) (Variable Deck)

prepareMusic :: FilePath -> System s Music
prepareMusic path = do
  wav <- readWAVE path
  i <- new $ variable $ source .~ sampleSource wav $ Deck.empty
  linkAudio $ playbackOf i
  return i
```

`readWAVE` は `.wav` ファイルから音楽をロードします。 `source .~ sampleSource wav $ Deck.empty` の部分が少々トリッキーです。

Deck は音楽を再生するためのユーティリティです。 `source` は `Lens` で純粋で関数型なアクセサの表現です。 `new $ variable $ v` が音楽を初期化します。 `linkAudio $ playbackOf i` は今はおまじないとでも思っててください。

### コンポーネント: getPosition と playMusic

`getPosition` と `playMusic` の実装は以下の通りです：

```haskell
getPosition :: Music s -> System s Time
getPosition m = m .- use pos

playMusic :: Music s -> System s ()
playMusic m = m .- playing .= True
```

ここで新しく2つの演算子が登場します： `use` と `.=` です。これらは `lens` ライブラリからです。このパッケージは様々のアクセサを扱うための型やユーティリティを含んでいます。

`pos` と `playing` は `Lens` です。 `Lens' s a` では `a` の値の取得の操作を `s` から出来ます。

```haskell
pos :: Lens' Deck Time
playing :: Lens' Deck Bool
```

`use` と `(.=)` はステートフルモナドに対して使える値を取得・設定するための演算子です。

```haskell
use :: MonadState s m => Lens' s a -> m a
(.=) :: MonadState s m => Lens' s a -> a -> m ()
```

lens を利用すればオブジェクトの一部へのアクセスを容易にできて、オブジェクト指向言語でいうメンバー変数のようなものを操作が出来ます。ですが deck のステートは `gameMain` の `music` にパックされているので直接は操作できません。 `objective` パッケージの `(.-)` 演算子は特定の操作を実行できます。

`getPosition m` は音楽 `m` からの経過時間を秒数で正確に返します。

ここまでのソースをまとめたのがこれです `src/tutorial-passive.hs` 。

```shell
$ dist/build/tutorial-passive/tutorial-passive
```

ですがまだゲームではありません。スコアもインタラクションもないです。

### 入力処理

入力の処理をしていきましょう。ここで新たに2つのコンポーネントを紹介します、 `rate` と `handle` です。

```haskell
rate :: Time -> Int
rate dt
  | dt < 0.05 = 4
  | dt < 0.1 = 2
  | otherwise = 1

handle :: Time -> Set Time -> (Int, Set Time)
handle t ts = case viewNearest t ts of
  Nothing -> (0, ts) -- 曲は終了
  Just (t', ts') -> (rate $ abs (t - t'), ts')
```

`rate` はタイムラグを元にスコアを計算します。 `handle` はスコアと更新されたタイミングを返します。 `viewNearest :: (Num a, Ord a) => a -> Set a -> (a, Set a)` はセットから最も近い値を返す関数です。もし一番近い値を返すのに失敗したらでたらめにボタンを押すことで予期せぬスコアの増加を許してしまいます。

```haskell
data Chatter a = Up a | Down a
```

以下のコードがイベントの処理をします。

```haskell
linkKeyboard $ \ev -> case ev of
  Down KeySpace -> do
    t <- getPosition
    ts <- timings .- get
    (sc, ts') <- handle t ts
    timings .- put ts'
    score .- modify (+sc)
  _ -> return () -- 他のイベントは無視する
```

いくつかの変数が初期化されています。

```haskell
timings <- new $ variable (allTimings !! 0)
score <- new $ variable 0
```

`linkKeyboard` が呼ばれる時、エンジンはキーボードのイベントを `Key` へ送ります。 `Key` はキーが押されてるか離されたかを識別するため `Chatter` によりラップされています。スペースキーが押された時、タイムラグを一番近いタイミングから計算し、正確度によってスコアを増加させます。

プレイヤーに現在のスコアを表示するために _フォント_ もロードしないといけません。 `Call.Util.Text.simple` は与えられたテキストを描画する関数を返してくれます。

```haskell
text <- Text.simple defaultFont 24 -- text :: String -> Picture
```

`renderGame` に `text (show sc)` を追加するだけです。現時点のインタラクションを追加したソースはこちらです `src/tutorial-active.hs` 。ゲームですね！わーい！

```shell
$ dist/build/tutorial-passive/tutorial-active
```

![tutorial-active](images/tutorial-active-screenshot.png)

### ゲームを拡張する

実際に遊んでみるとガッカリしちゃうでしょう。インタラクションがまだあまりよくないからです。もっとかっこいいエフェクトとかあれば楽しくなります。最近のリズムゲームは判定をすぐさま表示します。そうすればプレイヤーは自分がちゃんとプレイが上手いかどうかすぐわかります。

純粋関数型なデザインのおかげさまでレーンを簡単に拡張できます（ `tutorial-extended.hs` ）！

![extended](images/extended.png)

`ix i` は リストの `i`番目の項目を指す lens です。 `forM` の結果を `translate` を使って配置すればよいです。

他に面白いのが `transit` でアニメーションを作るのに便利です。

```haskell
pop :: Bitmap -> Object (Request Time Picture) Maybe
pop bmp = Control.Object.transit 0.5 $ \t -> translate (V2 320 360)
  $ translate (V2 0 (-80) ^* t)
  $ color (V4 1 1 1 (realToFrac $ 1 - t))
  $ bitmap bmp
```

引数 `t` は0.5秒間隔で0から1の間まで変化します。初期化するにはこのオブジェクトをリストに追加してください：

```haskell
effects <- new $ variable []
effects .- modify (pop _perfect_png:)
```

`effects .- announceMaybe (request dt)` は使われなくなったアニメーションを破棄しながら `[Picture]` を返します。 `objective` のおかげで色々得しています。以下が `linkPicture` の部分です：

```haskell
linkPicture $ \_ -> do
  [l0, l1, l2] <- forM [0..2] $ \i -> renderLane <$> (timings .- use (ix i)) <*> getPosition music
  s <- score .- get
  ps <- effects .- announceMaybe (request dt)
  return $ translate (V2 (-120) 0) l0
    <> translate (V2 0 0) l1
    <> translate (V2 120 0) l2
    <> color black (translate (V2 240 40) (text (show s)))
    <> mconcat ps
```

入力周りは難しいところはありません。

```haskell
let touchLane i = do
      ((sc, obj), ts') <- handle <$> getPosition music <*> (timings .- use (ix i))
      effects .- modify (obj:)
      timings .- ix i .= ts'
      score .- modify (+sc)

linkKeyboard $ \ev -> case ev of
  Down KeySpace -> touchLane 1
  Down KeyF -> touchLane 0
  Down KeyJ -> touchLane 2
  _ -> return () -- 他のイベントは無視する
```

GHC拡張の `LambdaCase` のおかげで `\ev -> case ev of` を `\case` と置き換えることができます。

ゲーム全体でたったの120行です！

```shell
$ wc -l src\tutorial-extended.hs
120
$ dist/build/tutorial-passive/tutorial-extended
```

パート III: 技術的背景
-----------------

### グラフィックス

合成可能な物で "empty" を含む物のことをモノイドと呼びます。__空の画像__ やオーバーレイすることにより __画像の合成__ が可能なので画像もモノイドです。標準ライブラリの `base` はモノイドの型クラスを定義しています：

```haskell
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
```

Call は __フリーモノイド__ を利用して画像を表現しています。

CPS(継続渡しスタイル)ではなく表現すると、

```haskell
data Scene = Empty
  | Combine Scene Scene
  | Primitive Bitmap PrimitiveMode (Vector Vertex) -- プリミティブを描画する
  | VFX (VFX Scene) -- 視覚エフェクトを適用する
  | Transform (M44 Float) Scene -- 行列を使って `Scene` を変換する
```

モノイドインスタンスは単純です。

```haskell
instance Monoid Scene where
  mempty = Empty
  mappend = Combine
```

フリーモノイドを利用すると描画の部分を `Scene` と切り分けることができます。 `drawScene :: Scene -> IO ()` はAPIを利用してSceneを描画します。空の画像の場合何もしません。 `Combine a b` は `drawScene a >> drawScene b` を呼ぶのと同義です。

`drawScene` の実装は以下のようになります：

```haskell
drawScene Empty = return ()
drawScene (Combine a b) = drawScene a >> drawScene b
drawScene (Primitive b m vs) = drawPrimitive b m vs
drawScene (VFX v) = drawScene (applyVFX v)
drawScene (Transform mat s) = withMatrix mat (drawScene s)
```

`drawPrimitive`、 `applyVFX`、 `withMatrix` は環境依存です。

free structure はドメイン固有言語の一種でプログラムの再利用を促進します。Andres Löh氏の [Monads for free!](https://skillsmatter.com/skillscasts/4430-monads-for-free) は free structure について勉強したいならオススメです。

Call は様々な変換を `Affine` クラスに定義しています。型族のおかげさまで同じ変換を2Dと3Dで利用できます。 `Normal` は法線ベクトルで3Dでは三次元ベクトルですが2Dではただの `Float` です。

```haskell
class Affine a where
  type Vec a :: *
  type Normal a :: *
  rotateOn :: Normal a -> a -> a
  scale :: Vec a -> a -> a
  translate :: Vec a -> a -> a
```

### オーディオ

現在、多くの環境で簡単にインストールできるようなオーディオのパッケージは少ないです。その中で私は様々なバックエンドをサポートする `portaudio` を選択しました。

人間は音に敏感でほんの20ミリ秒の誤差でも気づきます。故に、音ズレは特に最小限に抑えたいです。これが Call がコールバックを利用する最大の理由です。Callは軽量で丈夫なライブラリを目指していて、抽象化は `objective` に任せています。

謝辞
-------------------------
山本和彦さんにこのチュートリアルの設計を手伝っていただきました。
