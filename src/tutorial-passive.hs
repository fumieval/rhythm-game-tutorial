{-# LANGUAGE Rank2Types, ImpredicativeTypes #-}
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

gameMain :: System s ()
gameMain = do
  music <- prepareMusic

  linkPicture $ \_ -> renderGame allTimings <$> getPosition music

  playMusic music

main = runSystemDefault (gameMain >> stand)

type Music s = Inst' (StateT Deck (System s)) (System s)

prepareMusic :: System s (Music s)
prepareMusic = do
  wav <- readWAVE "assets/Monoidal Purity.wav"
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

circles :: [Float] -> Picture
circles = foldMap (\p -> V2 320 ((1 - p) * 480) `translate` bitmap circle_png)

renderGame :: Set Time -> Time -> Picture
renderGame ts t = mconcat [color blue $ circles (phases ts 1 t)
    , V2 320 480 `translate` color black (bitmap circle_png) -- criterion
    ]

getPosition :: Music s -> System s Time
getPosition m = m .- use pos

playMusic :: Music s -> System s ()
playMusic m = m .- playing .= True

parseScore :: Time -> String -> [Set Time]
parseScore d = map (Set.fromAscList . concat . zipWith (map . (+)) [0,d..]) . Data.List.transpose . map (map f) . splitWhen (=="") . lines where
  f l = [t | (t, c) <- zip [0, d/fromIntegral (length l)..] l, c == '.']

allTimings :: Set Time
allTimings = (!!1) $ unsafePerformIO $ parseScore (60/160*4) <$> readFile "assets/Monoidal Purity.txt"
