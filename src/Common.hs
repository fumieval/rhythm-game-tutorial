{-# LANGUAGE LambdaCase, MultiWayIf, ViewPatterns #-}
module Common where

import Data.Set (Set)
import qualified Data.Set as Set
import Paths_rhythm_game_tutorial
import Call
import System.IO.Unsafe
import Data.Foldable (foldMap)

type Timings = Set Time

circle_png :: Bitmap
circle_png = unsafePerformIO $ readBitmap "assets/circle.png"

-- | Eradicate circles.
decay :: Time -> Set Time -> Set Time
decay t = snd . Set.split t

phases :: Set Time -- ^ timings
    -> Time -- ^ life span
    -> Time -- ^ the current time
    -> [Float] -- ^ phase
phases s len t = map ((/len) . subtract t) -- transform to an interval [0, 1]
  $ Set.toList
  $ fst $ Set.split (t + len) s -- before the limit

circles :: [Float] -> Picture
circles = foldMap (\p -> V2 320 ((1 - p) * 480) `translate` circle_png)

allTimings :: Set Time
allTimings = Set.fromList $ concat $ zipWith (\t ch -> [t | ch == '*'])
  (iterate (+(60/160/2)) 0) -- the list of timings where circles may spawn
  "----------------*-*******-*-*-*-" -- asterisk indicates that a circle can spawn

viewNearest :: (Num a, Ord a) => a -> Set a -> Maybe (a, Set a)
viewNearest t ts = case Set.split t ts of
  (sa@(Set.maxView -> Just (a, sa')), sb@(Set.minView -> Just (b, sb')))
    | t - a < b - t -> Just (a, sa' `Set.union` sb)
    | otherwise -> Just (b, sa `Set.union` sb')
  (Set.maxView -> Just (a, sa'), _) -> Just (a, sa')
  (_, Set.minView -> Just (b, sb')) -> Just (t, sb')
  _ -> Nothing