{-# LANGUAGE FlexibleContexts #-}
import Call
import Audiovisual.Deck as Deck
import Control.Monad.State.Strict
import Control.Lens

main = runCallDefault $ do
  music <- prepareMusic "assets/Monoidal Purity.wav"
  playMusic music
  stand

type Music = Instance (StateT (Deck Stereo) IO) IO

prepareMusic :: Call => FilePath -> IO Music
prepareMusic path = do
  wav <- readWAVE path
  i <- new $ variable $ source .~ sampleSource wav $ Deck.empty
  linkAudio $ \dt n -> i .- playback dt n
  return i

playMusic :: Music -> IO ()
playMusic m = m .- playing .= True
