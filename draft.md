Creating rhythm game with Haskell
====
Fumiaki Kinoshita (part-time employee of IIJ-II)

Introduction
----
Rhythm games, also known as rhythm actions, are very popular genre in Japan. Konami released __Dance Dance Revolution (DDR) in 1998__ -- it is the best successful game among the genre. Another famous one, _Taiko no Tatsujin_(literally, __Expert of Japanese drum__) is being immensely supported by broad age-group of people. Today, various forms of rhythm games have been released one after another.

However, there are few tutorials to create such kind of games. Even if there are, they might be too old, or written in non-English, or/and work only in Windows.

This tutorial focuses on creating a rhythm game without pain. Don't be surprised, we use Haskell to do that.

This tutorial has two parts.

* In Part I, we build a very simple rhythm game. We use the Call engine to develop.
* Part II introduces some technical backgrounds (graphics, audio) that support Part I.

I'd be happy if this tutorial helps your curiosity to create a game.

Part I: Preparation
----
Firstly, we have to ensure that you have installed GHC. [Haskell Platform](https://www.haskell.org/platform/) is an easy way to install GHC. Note that using 32-bit version of GHC is safer to avoid problems even if your platform is Windows x64.

Very well, let's build `bindings-portaudio`:

> $ cabal install bindings-portaudio

If it fails, please check if the development library for the backend (e.g. libasound2-dev, libportaudio19) is installed. On windows, the installation is a bit complex. Extracted from `bindings-portaudio` README:

1. Download and unpack the latest [portaudio](http://www.portaudio.com/download.html) at a clear directory (e.g. `C:\portaudio`).
2. Run `bash configure`, then `make`.
3. Edit `portaudio-2.0` as follows:
```
--- prefix=/usr/local
+++ prefix=C:/portaudio

--- libdir=${exec_prefix}/lib
+++ libdir=${exec_prefix}/lib/.libs
```
4. If you don't have pkgconfig, download `pkg-config` from [GTK+ Download: Windows (32-bit)](http://www.gtk.org/download/win32.php) and make sure `pkg-config.exe` is in your `PATH`.
5. `set PKG_CONFIG_PATH=C:/portaudio`
6. Run `cabal update && cabal install bindings-portaudio`.

If it throws up something messy, please report to [the GitHub repository](https://github.com/fumieval/bindings-portaudio/issues).

Then install `call`.

> $ cabal install call

Part II: Creating a game
-------------------------------------------------

> Here we bang! -- Wada-don, "Taiko no Tatsujin"

Now, think of a very simple game: There's a circle, and another circle(s) is approaching. You hit the space key in exact timing when the another circle overlapped the original one. How do we implement this? The structure of the program can be derived by writing components and relationships down.

* Game has a picture which depends on the time.
* A music is playing through the game.

We want to synchronize graphics with the music. An action that returns the current time in the _music_ is neccesary: even if the programm has stumbled accidentally, the graphics and music shouldn't diverge. In call, actions are performed on `System s` monad. Let's divide the game into the following six components:

```haskell
prepareMusic :: System s Music
playMusic :: Music -> System s ()
getTime :: Music -> System s Time
allTimings :: Timings
renderGame :: Timings -> Time -> Picture
gameMain :: System s ()
```

Compose them like this.

```haskell
gameMain :: System s ()
gameMain = do
  music <- prepareMusic

  linkPicture $ \_ -> renderGame allTimings <$> getTime music

  playMusic music

main = runSystemDefault (gameMain >> stand)
```

`linkPicture :: (Time -> System s Picture) -> System ()` is the only function provided by Call to actually draw something.`linkPicture f` repeatedly calls `f` and draws the result of `f` to the window. The argument of `f` is the time difference between frames, it is often negilible though.

`runSystemDefault` runs `System s` in IO.

### Component: prepareMusic

A music is essential for rhythm games.

```haskell
import Control.Monad.State.Strict
import Call
import Call.Util.Deck as Deck

type Music = Inst' (State Deck) (System s)

prepareMusic :: System s Music
prepareMusic = do
  wav <- readWAVE "assets/Monoidal Purity.wav"
  i <- new $ variable $ source .~ sampleSource wav $ Deck.empty
  linkAudio $ playbackOf i
  return i
```

`readWAVE` loads a sound from `.wav` file.`source .~ sampleSource wav $ Deck.empty` is a bit tricky.

Deck is an utility to play a music. `source` is a `Lens` which is purely functional representation of accessors. This article explains about `Lens` later, in Part III.

`new $ variable $ v` instantiates a music. Regard `linkAudio $ playbackOf i` as a cliché for now.

We need to implement just `renderGame`. First, express timings as a set of time. Given timings and "life span" of circles, we can compute positions of visible circles from the current time.

### Component: renderGame

```haskell
phases :: Set Time -- ^ timings
    -> Time -- ^ life span
    -> Time -- ^ the current time
    -> [Float] -- ^ phase
phases s len t = map ((/len) . subtract t) -- transform to an interval [0, 1]
  $ Set.toList
  $ fst $ Set.split (t + len) -- before the limit
```

Create a function to render circles. Since `Picture` is a monoid, we can use `foldMap` or `mconcat` to combine pictures. `translate (V2 x y)` shifts the picture into (x, y).

```haskell
circle_png :: Bitmap
circle_png = unsafePerformIO $ readBitmap "assets/circle.png"

circles :: [Float] -> Picture
circles = foldMap (\p -> V2 320 ((1 - p) * 480) `translate` bitmap circle_png)
```

`renderGames` passes the result of `phases` into `circles`. Using `color`, we can change the color of picture.

```haskell
renderGame :: [Time] -> Time -> Picture
renderGame ts t = mconcat [color blue $ circles (phases ts 1 t)
    , V2 320 480 `translate` color black (bitmap circle_png) -- criterion
    ]
```

### Component: getTime and playMusic

The implementation of `getTime` and `playMusic` is as follows:

```haskell
getTime :: Music -> System s Time
getTime m = m .- use pos

playMusic :: Music -> System s ()
playMusic m = m .- playing .= True
```

You notice two new operators: `use` and `.=`. These comes from the `lens` library. This package contains types and utilities to deal with various accessors.

`pos`, `playing` are `Lens`. Given `Lens' s a`, you can take a value `a` from `s`, and you can update that.

```
pos :: Lens' Deck Time
playing :: Lens' Deck Bool
```

`use` and `(.=)` work on stateful monads.

```haskell
use :: MonadState s m => Lens' s a -> m a
(.=) :: MonadState s m => Lens' s a -> a -> m ()
```

With lens, we can access a specific element of a structure easily, allowing you manipulate just like "fields" in OOP languages. However, the state of the deck is packed in `music` in `gameMain` so these can't be used directly. `objective` provides an operator to resolve that. The `(.-)` operator passes state update of the right operand, conveying it to the left variable.

Putting them together, we got `src/tutorial-passive.hs`.

![tutorial-passive](images/tutorial-passive-screenshot.png)

It is not a game though -- simply because it has no score, no interaction.

### Handling inputs

Let's deal with inputs. Now introduce two components, `rateTiming` and `touch`.

```haskell
rate :: Time -> Int
rate dt
  | dt < 0.05 = 4
  | dt < 0.1 = 2
  | otherwise = 1

touch :: Time -> StateT Timings (System s) Int
touch t = do
  ts <- get
  case viewNearest t ts of
    Nothing -> return 0 -- The song is over
    Just (t', ts') -> do
      put ts'
      return $ rate $ abs (t - t')
```

`viewNearest :: (Num a, Ord a) => a -> Set a -> (a, Set a)` is a function to pick up the nearest value from a set.

```haskell
data Chatter a = Up a | Down a
```

All the input-related things is concentrated in the following:

```haskell
linkKeyboard $ \ev -> case ev of
  Down KeySpace -> do
    t <- getTime
    sc <- timings .- touch t
    score .- modify (+sc)
  _ -> return () -- Discard the other events
```

Note that a few variables has instantiated.

```
timings <- new $ variable allTimings
score <- new $ variable 0
```

After `linkKeyboard` is called, the engine passes keyboard events `Key`. `Key` is wrapped by `Chatter`, to indicate that a key is pressed, or released.

When the space key is pressed, it computes the time difference from the nearest timing, then increment the score by accuracy.

We need to load a _Font_ as we want to show players the current score. `Call.Util.Text.simple` generates a function that renders a supplied text.

```haskell
text <- Text.simple defaultFont 12 -- text :: String -> Picture
```

Just add `text (show sc)` to `renderGame`. `src/tutorial-active.hs` is the updated source we made interactive. It's a game, yay!

![tutorial-active](images/tutorial-active-screenshot.png)

However, when you actually play this, you may feel dissatisfied. It is because the interaction is still poor. If it would have more showy effects, it'll be exciting. Most rhythm games shows the recent evaluation of accuracy immediately. so players can notice whether their playing is good or bad.

Part III: Technical background
-----------------

### Graphics

Monoid is the general term for composable stuff which has "empty". A picture is one of the monoidal structures since there is a empty picture and picture can be composed by overlaying. The standard library provides a typeclass for monoids:

```haskell
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
```

Call uses _free monoid_ to represent picture.

In de-CPSed form,

```haskell
data Scene = Empty
  | Combine Scene Scene
  | Primitive Bitmap PrimitiveMode (Vector Vertex) -- draw a primitive
  | VFX (VFX Scene) -- apply visual effects
  | Transform (M44 Float) Scene -- transform `Scene` using a matrix
```

Its Monoid instance is trivial.

```haskell
instance Monoid Scene where
  mempty = Empty
  mappend = Combine
```

Using free monoid, we can isolate the drawing process from `Scene`. Think of `drawScene :: Scene -> IO ()` which calls concrete APIs to draw Scene. For empty picture, we don't do nothing. `Combine a b` is equivalent to calling `drawScene a >> drawScene b`.

So the implementation of `drawScene` will be as follows:

```haskell
drawScene Empty = return ()
drawScene (Combine a b) = drawScene a >> drawScene b
drawScene (Primitive b m vs) = drawPrimitive b m vs
drawScene (VFX v) = drawScene (applyVFX v)
drawScene (Transform mat s) = withMatrix mat (drawScene s)
```

where `drawPrimitive`, `applyVFX`, `withMatrix` is environment-dependent.

In other words, free structures is a kind of DSL which encourages the reusability and independence of programs. Andres Löh's [Monads for free!](https://skillsmatter.com/skillscasts/4430-monads-for-free) is a great introduction for free structures.

Call puts together a few kinds of transformation in `Affine` class. Thanks to type families, we can use the same operation for both 2D and 3D. `Normal` is the normal vector, which is 3-dimensional vector in 3D but it is just `Float` in 2D.

```haskell
class Affine a where
  type Vec a :: *
  type Normal a :: *
  rotateOn :: Normal a -> a -> a
  scale :: Vec a -> a -> a
  translate :: Vec a -> a -> a
```

### Audio

Currently, there are few packages for audio that work in common platforms and are easy to install. I choosed `portaudio` for now which supports a bunch of backends. Humans are so sensitive about sound. 20 miliseconds of latency is noticable for us.

Thus, it is important to minimize latency when it comes to audio. The raw `portaudio` uses the callback model. This is the main reason of why call relies on callback. `objective` package contributes to relax the pain of handling events and states. The call library aims to be small and concrete so that it only provides a way to interact with the machine.
