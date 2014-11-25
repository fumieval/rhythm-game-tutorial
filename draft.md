Creating rhythm game with Haskell
====
Fumiaki Kinoshita (part-time employee of IIJ-II)

Introduction
----
Rhythm games, also known as rhythm actions, are very popular genre in Japan. Konami released __Dance Dance Revolution (DDR) in 1998__ -- it is the best successful game among the genre. Another famous one, _Taiko no Tatsujin_(literally, __Expert of Japanese drum__) is being immensely supported by broad age-group of people. Today, various forms of rhythm games have been released one after another.

However, there are few tutorials to create such kind of games. Even if there are, they might be too old, or written in non-English, or/and work only in Windows.

This tutorial focuses on creating a rhythm game without the pain of manipulating audio stuff. Don't be surprised, we use Haskell to do that.

This tutorial has two parts.

* In Part I, let us make a very simple rhythm game. We use the Call engine to develop.
* Part II introduces some technical backgrounds (graphics, signal processing, lens and so on) that support Part I.

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

If it fails, please check if the development library for the backend (e.g. libasound2-dev) is installed. If it throws up something messy, please report to [the GitHub repository](https://github.com/fumieval/bindings-portaudio/issues).

Then let's install `call`.

> $ cabal install call

Now, think of a very simple game: There's a circle, and another circle(s) is approaching. You touch　in exact timing when the another circle overlapped the original one. How do we implement this? 

First, express timings as a list of time. Given timings and "life span" of circles, we can compute positions of visible circles from the current time.

```haskell
phases :: [Time] -- ^ timings
    -> Time -- ^ life span
    -> Time -- ^ the current time
    -> [Float] -- ^ phase
```

I prepared a music and [Time]. Let's knit them into a game.

First, create a function to render circles.

```haskell
circles :: [Float] -> Picture
circles = foldMap (\p -> V2 320 ((1 - p) * 480)  `translate` circleOutline 48)
```

```
renderGame :: [Time] -> Time -> Picture
renderGame ts t = mconcat [color blue $ circles (phases ts 1 t)
    , translate (V2 320 480) `translate` color black (circleOutline 48)]
```

Putting them together. It is easy, isn't it?

```haskell
import Call
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Data.Foldable (foldMap)

phases :: Set Time -- ^ timings
    -> Time -- ^ life span
    -> Time -- ^ the current time
    -> [Float] -- ^ phase
phases s len t = map ((/len) . subtract t) -- transform to an interval [0, 1]
  $ Set.toList
  $ fst $ Set.split (t + len) -- before the limit
  $ snd $ Set.split t s -- after the current time

renderGame :: Set Time -> Time -> Picture
renderGame ts t = mconcat [color blue $ circles (phases ts 1 t)
    , V2 320 480 `translate` color black (circleOutline 48)]

circles :: [Float] -> Picture
circles = foldMap (\p -> V2 320 ((1 - p) * 480) `translate` circleOutline 48)

timings :: Set Time
timings = Set.fromList $ concat $ zipWith (\t ch -> [t | ch == '*'])
  (iterate (+(60/160/2)) 1) -- the list of timings where circles may spawn
  "*-*******-*-*-*-" -- asterisk indicates that a circle can spawn

main = runSystemDefault $ do
  time <- new $ variable 0
  linkPicture $ \dt -> do
    t <- time .- get
    time .- put (t + dt)
    return $ renderGame timings t
  stand
```

It's time to get it to interact. `call` provides `linkKeyboard` which takes a handler.

```haskell
linkKeyboard :: (Chatter Key -> System s ()) -> System s ()
```

`Key` is wrapped by `Chatter`, to indicate that a key is pressed, or released. In other words, it is a _vertical_ Either, where the original Either is about left and right.

```haskell
data Chatter a = Up a | Down a
```