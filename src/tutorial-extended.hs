{-# LANGUAGE Rank2Types, ImpredicativeTypes, ViewPatterns #-}
import Call
import Call.Util.Deck as Deck
import Control.Lens
import Control.Monad.State.Strict
import Data.Foldable (foldMap)
import qualified Data.Set as Set
import Data.Set (Set)
import System.IO.Unsafe
import Data.List
import Data.List.Split (splitWhen)
import Call.Util.Text as Text
import Data.Functor.Request
import Control.Object

gameMain :: System s ()
gameMain = do
  music <- prepareMusic "assets/Monoidal Purity.wav"

  allTimings <- liftIO $ parseScore (60/160*4) <$> readFile "assets/Monoidal Purity.txt"

  text <- Text.simple defaultFont 24

  timings <- new $ variable allTimings
  score <- new $ variable 0
  effects <- new $ variable []

  linkPicture $ \dt -> do
    [l0, l1, l2] <- forM [0..2] $ \i -> renderLane <$> (timings .- use (ix i)) <*> getPosition music
    s <- score .- get
    ps <- effects .- announceMaybe (request dt)
    return $ translate (V2 (-120) 0) l0
      <> translate (V2 0 0) l1
      <> translate (V2 120 0) l2
      <> color black (translate (V2 240 40) (text (show s)))
      <> mconcat ps

  let touchLane i = do
        ((sc, obj), ts') <- handle <$> getPosition music <*> (timings .- use (ix i))
        effects .- modify (obj:)
        timings .- ix i .= ts'
        score .- modify (+sc)

  linkKeyboard $ \ev -> case ev of
    Down KeySpace -> touchLane 1
    Down KeyF -> touchLane 0
    Down KeyJ -> touchLane 2
    _ -> return () -- Discard the other events

  playMusic music

main = runSystemDefault (gameMain >> stand)

type Music s = Inst (System s) (StateT Deck (System s)) (System s)

prepareMusic :: FilePath -> System s (Music s)
prepareMusic path = do
  wav <- readWAVE path
  i <- new $ variable $ source .~ sampleSource wav $ Deck.empty
  linkAudio $ \dt n -> i .- playback dt n
  return i

phases :: Set Time -- ^ timings
    -> Time -- ^ life span
    -> Time -- ^ the current time
    -> [Float] -- ^ phase
phases s len t = map ((/len) . subtract t) -- transform to an interval [0, 1]
  $ Set.toList
  $ fst $ Set.split (t + len) s -- before the limit

circle_png :: Bitmap
circle_png = unsafePerformIO $ readBitmap "assets/circle.png"

_perfect_png = unsafePerformIO $ readBitmap "assets/perfect.png"
_good_png = unsafePerformIO $ readBitmap "assets/good.png"
_error_png = unsafePerformIO $ readBitmap "assets/error.png"

circles :: [Float] -> Picture
circles = foldMap (\p -> V2 320 ((1 - p) * 480) `translate` bitmap circle_png)

renderLane :: Set Time -> Time -> Picture
renderLane ts t = mconcat [color blue $ circles (phases ts 1 t)
    , V2 320 480 `translate` color black (bitmap circle_png) -- criterion
    ]

getPosition :: Music s -> System s Time
getPosition m = m .- use pos

playMusic :: Music s -> System s ()
playMusic m = m .- playing .= True

parseScore :: Time -> String -> [Set Time]
parseScore d = map (Set.fromAscList . concat . zipWith (map . (+)) [0,d..]) . Data.List.transpose . map (map f) . splitWhen (=="") . lines where
  f l = [t | (t, c) <- zip [0, d/fromIntegral (length l)..] l, c == '.']

rate :: Time -> (Int, Object (Request Time Picture) Maybe)
rate dt
  | dt < 0.05 = (4, pop _perfect_png)
  | dt < 0.1 = (2, pop _good_png)
  | otherwise = (1, pop _error_png)

handle :: Time -> Set Time -> ((Int, Object (Request Time Picture) Maybe), Set Time)
handle t ts = case viewNearest t ts of
  Nothing -> ((0, pop _error_png), ts) -- The song is over
  Just (t', ts') -> (rate $ abs (t - t'), ts')

viewNearest :: (Num a, Ord a) => a -> Set a -> Maybe (a, Set a)
viewNearest t ts = case Set.split t ts of
  (sa@(Set.maxView -> Just (a, sa')), sb@(Set.minView -> Just (b, sb')))
    | t - a < b - t -> Just (a, sa' `Set.union` sb)
    | otherwise -> Just (b, sa `Set.union` sb')
  (Set.maxView -> Just (a, sa'), _) -> Just (a, sa')
  (_, Set.minView -> Just (b, sb')) -> Just (t, sb')
  _ -> Nothing

pop :: Bitmap -> Object (Request Time Picture) Maybe
pop bmp = Control.Object.transit 0.5 $ \t -> translate (V2 320 360)
  $ translate (V2 0 (-80) ^* t)
  $ color (RGBA 1 1 1 (realToFrac $ 1 - t))
  $ bitmap bmp
