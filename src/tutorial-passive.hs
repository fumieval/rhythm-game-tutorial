import Call
import Control.Monad.State
import qualified Call.Util.Deck as Deck
import Control.Lens
import Common

renderGame :: Timings -> Time -> Picture
renderGame ts t = mconcat [color blue $ circles (phases ts 1 t)
    , V2 320 480 `translate` color black (circleOutline 48)]

main = runSystemDefault $ do
  -- Music preparation
  wav <- readWAVE "assets/Monoidal Purity.wav"
  deck <- new $ variable $ Deck.source .~ sampleSource wav $ Deck.empty
  linkAudio $ \dt n -> deck .- Deck.playback dt n

  timings <- new $ variable allTimings

  linkPicture $ \dt -> do
    t <- deck .- use Deck.pos
    ts <- timings .- get
    timings .- put (decay (t - 1) ts)
    return $ renderGame ts t
  deck .- Deck.playing .= True -- Start the music
  
  stand