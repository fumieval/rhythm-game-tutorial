{-# LANGUAGE TemplateHaskell, LambdaCase, MultiWayIf, GADTs, OverloadedStrings, FlexibleContexts, TypeOperators, ViewPatterns, DataKinds, KindSignatures, Rank2Types #-}
import qualified Call.Util.Deck as D
import Call
import Control.Lens
import Control.Monad.State.Strict
import Data.Maybe
import System.IO.Unsafe
import Data.OpenUnion1.Clean
import qualified Data.Set as S
import Data.List.Split
import Data.List (transpose)
import Control.Arrow

_perfect_png = unsafePerformIO $ readBitmap "assets/perfect.png"
_good_png = unsafePerformIO $ readBitmap "assets/good.png"
_error_png = unsafePerformIO $ readBitmap "assets/error.png"
_glow_orange_png = unsafePerformIO $ readBitmap "assets/glow_orange.png"
_glow_blue_png = unsafePerformIO $ readBitmap "assets/glow_blue.png"

flash :: Picture () -> Object Graphic Maybe
flash p = transit 0.1 $ \t -> opacity (realToFrac $ 1 - t) p

pop :: Bitmap -> Object Graphic Maybe
pop bmp = transit 0.5 $ \t -> translate (V2 0 (-80) ^* t) $ opacity (realToFrac $ 1 - t) $ bitmap bmp

effects0 :: ObjS (Request (Object Graphic Maybe) () |> Graphic |> Nil) s
effects0 = stateful match [] where
  match :: forall s a. (Request (Object Graphic Maybe) () |> Graphic |> Nil) a
    -> StateT [Object Graphic Maybe] (System s) a
  match = (\(Request obj) -> modify (obj:))
    ||> (\(Request dt) -> liftM (translate (V2 240 240) . sequence_) $ announce $ request dt)
    ||> exhaust

gameCore :: Lift (Request (Object Graphic Maybe) ()) e => [S.Set Time] -> AddrS D.Methods s -> AddrS e s -> ObjS (Request Int ()) s
gameCore notes deck effects = liftO $ \(Request i) -> do
  case i of
    0 -> effects .<< flash (rotateD 45 $ translate (V2 0 (-100)) $ bitmap _glow_orange_png)
    1 -> effects .<< flash (translate (V2 0 (-100)) $ bitmap _glow_blue_png)
    2 -> effects .<< flash (rotateD (-45) $ translate (V2 0 (-100)) $ bitmap _glow_orange_png)

  t0 <- deck .& use D.pos
  let t = t0
  let dt = case S.maxView *** S.minView $ S.split t (notes !! i) of
        (Just (a, _), Just (b, _)) -> min (t - a) (b - t)
        (Just (a, _), _) -> t - a
        (_, Just (b, _)) -> t - b
        _ -> 1
  liftIO $ print dt
  if
    | dt < 0.05 -> effects .<< pop _perfect_png
    | dt < 0.10 -> effects .<< pop _good_png
    | otherwise -> effects .<< pop _error_png

musicView :: [S.Set Time] -> AddrS D.Methods s -> ObjS Graphic s
musicView notes deck = liftO $ \(Request dt) -> do
  t0 <- deck .& use D.pos
  let t = t0
  return $ translate (V2 240 240) $ thickness 1.5 $ do
    color black $ circleOutline 32
    color "F7931E" $ toScore 1 (notes !! 0) (\s -> translate (V2 (-170) (-170) ^* s)) t
    color "1B1464" $ toScore 1 (notes !! 1) (\s -> translate (V2 0 (-240) ^* s)) t
    color "F7931E" $ toScore 1 (notes !! 2) (\s -> translate (V2 170 (-170) ^* s)) t

toScore :: Double -- ^ Note visible duration
  -> S.Set Time -- ^ Notes
  -> (Double -> Picture () -> Picture ()) -- ^ Note animation
  -> Time
  -> Picture ()
toScore noteD notes f t = do
  let ns = map ((/noteD) . subtract t) $ S.toList $ fst $ S.split (t + noteD) $ snd $ S.split t notes
  forM_ ns $ \s -> f s $ circleOutline 32

handleInput :: AddrS (Request Int ()) s -> ObjS Keyboard s
handleInput core = liftO $ \case
  Request (Down KeyF) -> core .<< 0
  Request (Down KeySpace) -> core .<< 1
  Request (Down KeyJ) -> core .<< 2
  Request _ -> return ()

parseScore :: Time -> String -> [S.Set Time]
parseScore d = map (S.fromAscList . concat . zipWith (map . (+)) [0,d..]) . transpose . map (map f) . splitWhen (=="") . lines where
  f l = [t | (t, c) <- zip [0, d/fromIntegral (length l)..] l, c == '.']

main = runSystemDefault $ do
  score <- liftIO $ parseScore (60/160*4)<$> readFile "Monoidal Purity.txt"
  deck <- new D.empty
  es <- new effects0
  linkGraphic es
  c <- new $ gameCore score deck es
  new (handleInput c) >>= linkKeyboard
  new (musicView score deck) >>= linkGraphic

  linkAudio deck
  wav <- readWAVE "Monoidal Purity.wav"
  deck .& D.source ?= wav
  deck .& D.playing .= True
  stand

