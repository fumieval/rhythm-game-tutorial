import Call
import Control.Monad.State
import qualified Call.Util.Deck as Deck
import Control.Lens
import Common

main = runSystemDefault $ do
  -- Music preparation
  wav <- readWAVE "assets/Monoidal Purity.wav"
  deck <- new $ variable $ Deck.source .~ sampleSource wav $ Deck.empty
  linkAudio $ \dt n -> deck .- Deck.playback dt n

  linkPicture $ \dt -> do
    t <- deck .- use Deck.pos
    return $ renderGame allTimings t
  deck .- Deck.playing .= True -- start the music
  
  stand -- wait forever