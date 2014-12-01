{-# LANGUAGE LambdaCase, MultiWayIf, ViewPatterns #-}
import Call
import Control.Monad.State
import qualified Call.Util.Deck as Deck
import qualified Call.Util.Text as Text
import Control.Lens
import Common

renderGame :: Timings -> Time -> Picture
renderGame ts t = mconcat [color blue $ circles (phases ts 1.5 t)
    , V2 320 480 `translate` color black (bitmap circle_png)]

main = runSystemDefault $ do
  score <- new $ variable 0
  timings <- new $ variable allTimings

  wav <- readWAVE "assets/Monoidal Purity.wav"
  text <- Text.simple defaultFont 12

  deck <- new $ variable $ Deck.empty & Deck.source .~ sampleSource wav
  linkAudio $ \dt n -> deck .- Deck.playback dt n

  linkPicture $ \dt -> do
    t <- deck .- use Deck.pos
    ts <- timings .- get
    sc <- score .- get
    return $ renderGame ts t <> translate (V2 400 60) (color black $ text $ "Score: " ++ show sc)

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
  
  deck .- Deck.playing .= True -- Start the music

  stand