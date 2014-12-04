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

Part I: Get excited
----

> Here we bang! -- Wada-don, "Taiko no Tatsujin"

Firstly, we have to ensure that you have installed GHC. [Haskell Platform](https://www.haskell.org/platform/) is an easy way to install GHC. Note that using 32-bit version of GHC is safer to avoid problems even if your platform is Windows x64.

We have to choose some from several backends:

* If you are using Windows Vista or later, WASAPI is good choice. DirectSound is an another candidate.
* If you are Mac user, you probably like to use CoreAudio.
* On Linux or FreeBSD, we support ALSA or OSS or JACK. If you don't know the differences, choose ALSA for now.
* If you have an ASIO-compatible audio device, and you have registered to Steinberg as a 3rd-party developer, and you desire high-quality sound or low latency, using ASIO is worth.

Very well, let's build `bindings-portaudio`:

> $ cabal install bindings-portaudio -fWASAPI

Rewrite `WASAPI` according to your choice. If you choosed WASAPI, leave it as is, of course.

If it fails, please check if the development library for the backend (e.g. libasound2-dev, libportaudio19) is installed. If it throws up something messy, please report to [the GitHub repository](https://github.com/fumieval/bindings-portaudio/issues).

Then install `call`.

> $ cabal install call

Now, think of a very simple game: There's a circle, and another circle(s) is approaching. You touch　in exact timing when the another circle overlapped the original one. How do we implement this? 

First, express timings as a set of time. Given timings and "life span" of circles, we can compute positions of visible circles from the current time.

```haskell
phases :: Set Time -- ^ timings
    -> Time -- ^ life span
    -> Time -- ^ the current time
    -> [Float] -- ^ phase
phases s len t = map ((/len) . subtract t) -- transform to an interval [0, 1]
  $ Set.toList
  $ fst $ Set.split (t + len) -- before the limit
```

Create a function to render circles. Since `Picture` is a monoid, we can use `foldMap` to combine pictures. `translate (V2 x y)` shifts the picture into (x, y).

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

`new $ variable x` instantiates mutable variable. Don't worry -- the mutability appears only in `main` and all the others are **pure**. It is the great advantage of Haskell.

```haskell
main = runSystemDefault $ do
  -- Music preparation
  wav <- readWAVE "assets/Monoidal Purity.wav"
  deck <- new $ variable $ Deck.source .~ sampleSource wav $ Deck.empty
  linkAudio $ \dt n -> deck .- Deck.playback dt n

  timings <- new $ variable allTimings

  linkPicture $ \dt -> do
    t <- deck .- use Deck.pos -- get the accurate current time
    ts <- timings .- get
    timings .- put (decay (t - 1) ts)
    return $ renderGame ts t
  deck .- Deck.playing .= True -- Start the music
  
  stand
```

Putting them together, we got `src/tutorial-passive.hs`. It is easy, isn't it? It is not a game though -- simply because it has no score, no interaction.

Let's deal with inputs. `linkKeyboard` passes keyboard events to the supplied function. `Key` is wrapped by `Chatter`, to indicate that a key is pressed, or released.

```haskell
data Chatter a = Up a | Down a
```

All the input-related things is concentrated in the following:

```haskell
linkKeyboard $ \case
  Down KeySpace -> do
    ts <- timings .- get
    t <- deck .- use Deck.pos
    case viewNearest t ts of
      Nothing -> return () -- The song is over
      Just (t', ts') -> do
        let dt = abs (t - t')
        timings .- put ts'
        if
          | dt < 0.05 -> score .- modify (+4) -- Great!
          | dt < 0.1 -> score .- modify (+2) -- Good
          | otherwise -> score .- modify (+1) -- Bad...
  _ -> return () -- Discard the other events
```

`viewNearest :: (Num a, Ord a) => a -> Set a -> (a, Set a)` is a function to pick up the nearest value. Note that the GHC extensions `LambdaCase` and `MultiWayIf` is used in the code. After `linkKeyboard` is called, the engine passes keyboard events into the `\case` function. When the space key is pressed, it computes the time difference from the nearest timing. Increment the score by accuracy.

We need to load a _Font_ as we want to show players the current score. `Call.Util.Text.simple` generates a function that renders a supplied text.

```haskell
text <- Text.simple defaultFont 12 -- text :: String -> Picture
```

`src/tutorial-active.hs` is the updated source we made interactive. It's a game, yay!

However, when you actually play this, you may feel dissatisfied. It is because the interaction is still poor. If it would have more showy effects, it'll be exciting... OK, come along.

Most rhythm games shows the recent evaluation of accuracy immediately. so players can notice whether their playing is good or bad. It is what we're going to do.

Part II
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

In other words, free structures is a kind of DSL. They encourages the reusability, independence of programs. Andres Löh's [Monads for free!](https://skillsmatter.com/skillscasts/4430-monads-for-free) is a great introduction of free structures.

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

Currently, there are few packages for audio that work in common platforms and are easy to install. I choosed `portaudio` for now which supports a bunch of backends.

`linkAudio` passes the number of frames and time delta to the function and plays the result waveform. It is quite generic, though using directly is difficult. Call offers two utilities: Deck and Sampler. Both of them are completely independent from the core of call. To use them, create a variable that contains initial state `empty`, and pass `playback` to `linkAudio`.

The type signature of `playback` shows they requires a stateful context of Sampler or Deck.

```haskell
playback :: MonadState Sampler m => Time -> Int -> m (V.Vector Stereo) 
```

`objective` provides an operator to resolve. now `deck` is a variable which has the state of the deck:

```haskell
sampler <- new $ variable Sampler.empty
```

The (.-) operator absorbs the state update of `playback`, conveying it to the variable `deck`. The same thing can be applied for `Sampler`.

```haskell
linkAudio $ \dt n -> sampler .- Sampler.playback dt n
```

`readWAVE` loads a sound from `.wav` file. To play, run `sampler .- play s`.

Call manipulates graphics, audio, and input in separate processes. Therefore, Call can be applied in time-critical applications such as rhythm games.