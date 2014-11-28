{-# LANGUAGE LambdaCase, MultiWayIf, ViewPatterns #-}
import Call
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Data.Foldable (foldMap)
import Control.Lens
import qualified Call.Util.Deck as Deck
import System.IO.Unsafe
import qualified Call.Util.Text as Text

circle_png :: Bitmap
circle_png = unsafePerformIO $ readBitmap "assets/circle.png"

phases :: Set Time -- ^ timings
    -> Time -- ^ life span
    -> Time -- ^ the current time
    -> [Float] -- ^ phase
phases s len t = map ((/len) . subtract t) -- transform to an interval [0, 1]
  $ Set.toList
  $ fst $ Set.split (t + len) s-- before the limit

renderGame :: Set Time -> Time -> Picture
renderGame ts t = mconcat [color blue $ circles (phases ts 1.5 t)
    , V2 320 480 `translate` color black (bitmap circle_png)]

circles :: [Float] -> Picture
circles = foldMap (\p -> V2 320 ((1 - p) * 480) `translate` bitmap circle_png)

timings :: Set Time
timings = Set.fromList $ concat $ zipWith (\t ch -> [t | ch == '*'])
  (iterate (+(60/160/2)) 0) -- the list of timings where circles may spawn
  "----------------*-*******-*-*-*-" -- asterisk indicates that a circle can spawn

-- | Eradicate circles.
decay :: Time -> Set Time -> Set Time
decay t = snd . Set.split t

viewNearest :: (Num a, Ord a) => a -> Set a -> Maybe (a, Set a)
viewNearest t ts = case Set.split t ts of
  (sa@(Set.maxView -> Just (a, sa')), sb@(Set.minView -> Just (b, sb')))
    | t - a < b - t -> Just (a, sa' `Set.union` sb)
    | otherwise -> Just (b, sa `Set.union` sb')
  (Set.maxView -> Just (a, sa'), _) -> Just (a, sa')
  (_, Set.minView -> Just (b, sb')) -> Just (t, sb')
  _ -> Nothing

main = runSystemDefault $ do
  score <- new $ variable 0
  timings <- new $ variable timings

  wav <- readWAVE "Monoidal Purity.wav"
  text <- Text.simple defaultFont 12

  deck <- new $ variable $ Deck.empty & Deck.source .~ sampleSource wav
  linkAudio $ \dt n -> deck .- Deck.playback dt n

  linkPicture $ \dt -> do
    t <- deck .- use Deck.pos
    ts <- timings .- get
    sc <- score .- get >>= text . show
    return $ renderGame ts t <> translate (V2 480 60) (color black sc)

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
            | dt < 0.05 -> score .- modify (+10) -- Great!
            | dt < 0.1 -> score .- modify (+7) -- Good
            | otherwise -> score .- modify (+1) -- Bad...
    _ -> return () -- Discard the other events
  
  deck .- Deck.playing .= True

  stand