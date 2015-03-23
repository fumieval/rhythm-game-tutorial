{-# LANGUAGE Rank2Types, FlexibleContexts #-}
import Call
import Audiovisual.Deck as Deck
import Control.Lens
import Control.Monad.State.Strict
import Data.Foldable (foldMap)
import qualified Data.Set as Set
import Data.Set (Set)
import System.IO.Unsafe
import Data.List
import Data.List.Split (splitWhen)

main = runCallDefault $ do
  music <- prepareMusic "assets/Monoidal Purity.wav"

  allTimings <- liftIO $ (!!0) <$> parseScore (60/160*4) <$> readFile "assets/Monoidal Purity.txt"

  linkPicture $ \_ -> renderLane allTimings <$> getPosition music

  playMusic music

  stand

type Music = Instance (StateT (Deck Stereo) IO) IO

prepareMusic :: Call => FilePath -> IO Music
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

circles :: [Float] -> Picture
circles = foldMap (\p -> V2 320 ((1 - p) * 480) `translate` bitmap circle_png)

renderLane :: Set Time -> Time -> Picture
renderLane ts t = mconcat [color blue $ circles (phases ts 1 t)
    , V2 320 480 `translate` color black (bitmap circle_png) -- criterion
    ]

getPosition :: Music -> IO Time
getPosition m = m .- use pos

playMusic :: Music -> IO ()
playMusic m = m .- playing .= True

parseScore :: Time -> String -> [Set Time]
parseScore d = map (Set.fromAscList . concat . zipWith (map . (+)) [0,d..]) . Data.List.transpose . map (map f) . splitWhen (=="") . lines where
  f l = [t | (t, c) <- zip [0, d/fromIntegral (length l)..] l, c == '.']
