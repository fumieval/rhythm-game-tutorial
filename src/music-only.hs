import Call
import Call.Util.Deck as Deck
import Control.Monad.State.Strict
import Control.Lens

main = runSystemDefault $ do
  music <- prepareMusic "assets/Monoidal Purity.wav"
  playMusic music
  stand

type Music s = Instance (StateT Deck (System s)) (System s)

prepareMusic :: FilePath -> System s (Music s)
prepareMusic path = do
  wav <- readWAVE path
  i <- new $ variable $ source .~ sampleSource wav $ Deck.empty
  linkAudio $ \dt n -> i .- playback dt n
  return i

playMusic :: Music s -> System s ()
playMusic m = m .- playing .= True
