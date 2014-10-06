{-# LANGUAGE TemplateHaskell, LambdaCase, MultiWayIf, GADTs, OverloadedStrings, FlexibleContexts, TypeOperators, ViewPatterns, ConstraintKinds, Rank2Types #-}
import qualified Call.Util.Deck as Deck
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
import Data.Maybe
import Data.Monoid

center = translate (V2 320 360)

type Stack e = Request (Object e Maybe) ()

stacker :: (Monad m, Monoid b) => Object (Stack (Request a b) |> Request a b |> Nil) m
stacker = stateful (acceptM (modify . (:)) ||> ann ||> exhaust) [] where
  ann (Request e cont) = state $ first (cont . mconcat) . unzip . catMaybes . map (flip runObject (request e))

_perfect_png = unsafePerformIO $ readBitmap "assets/perfect.png"
_good_png = unsafePerformIO $ readBitmap "assets/good.png"
_error_png = unsafePerformIO $ readBitmap "assets/error.png"
_glow_orange_png = unsafePerformIO $ readBitmap "assets/glow_orange.png"
_glow_blue_png = unsafePerformIO $ readBitmap "assets/glow_blue.png"

flash :: Picture () -> Object Graphic Maybe
flash p = transit 0.1 $ \t -> center $ opacity (realToFrac $ 1 - t) p

pop :: Bitmap -> Object Graphic Maybe
pop bmp = transit 0.5 $ \t -> center $ translate (V2 0 (-80) ^* t) $ opacity (realToFrac $ 1 - t) $ bitmap bmp

gameCore :: Lift (Stack Graphic) e
  => [S.Set Time] -> AddrS Deck.Methods s -> AddrS e s -> ObjS Keyboard s
gameCore notes deck effects = liftO $ accept $ \case
  Down k -> do
    i <- case k of
      KeyF -> effects .<< flash (rotateD 45 $ translate (V2 0 (-100)) $ bitmap _glow_orange_png) >> return 0
      KeySpace -> effects .<< flash (translate (V2 0 (-100)) $ bitmap _glow_blue_png) >> return 1
      KeyJ -> effects .<< flash (rotateD (-45) $ translate (V2 0 (-100)) $ bitmap _glow_orange_png) >> return 2
      _ -> return 0

    t <- deck .& use Deck.pos
    let dt = case S.maxView *** S.minView $ S.split t (notes !! i) of
          (Just (a, _), Just (b, _)) -> min (t - a) (b - t)
          (Just (a, _), _) -> t - a
          (_, Just (b, _)) -> t - b
          _ -> 1
    if
      | dt < 0.05 -> effects .<< pop _perfect_png
      | dt < 0.10 -> effects .<< pop _good_png
      | otherwise -> effects .<< pop _error_png
  _ -> return ()

musicView :: [S.Set Time] -> AddrS Deck.Methods s -> ObjS Graphic s
musicView notes deck = liftO $ accept $ \dt -> do
  t <- deck .& use Deck.pos
  return $ center $ thickness 1.5 $ do
    color black $ circleOutline 32
    color "F7931E" $ toScore 1 (notes !! 0) (\s -> translate (V2 (-255) (-255) ^* s)) t
    color "1B1464" $ toScore 1 (notes !! 1) (\s -> translate (V2 0 (-360) ^* s)) t
    color "F7931E" $ toScore 1 (notes !! 2) (\s -> translate (V2 255 (-255) ^* s)) t

toScore :: Double -- ^ Note visible duration
  -> S.Set Time -- ^ Notes
  -> (Double -> Picture () -> Picture ()) -- ^ Note animation
  -> Time -> Picture ()
toScore noteD notes f t = do
  let ns = map ((/noteD) . subtract t) $ S.toList $ fst $ S.split (t + noteD) $ snd $ S.split t notes
  forM_ ns $ \s -> f s $ circleOutline 32

parseScore :: Time -> String -> [S.Set Time]
parseScore d = map (S.fromAscList . concat . zipWith (map . (+)) [0,d..]) . transpose . map (map f) . splitWhen (=="") . lines where
  f l = [t | (t, c) <- zip [0, d/fromIntegral (length l)..] l, c == '.']

main = runSystemDefault $ do
  score <- liftIO $ parseScore (60/160*4)<$> readFile "Monoidal Purity.txt"
  deck <- new Deck.empty
  s <- new stacker
  linkGraphic s
  new (gameCore score deck s) >>= linkKeyboard
  new (musicView score deck) >>= linkGraphic
  linkAudio deck
  wav <- readWAVE "Monoidal Purity.wav"
  deck .& Deck.source ?= wav
  deck .& Deck.playing .= True
  stand