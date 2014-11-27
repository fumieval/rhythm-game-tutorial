import Call
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Data.Foldable (foldMap)

-- | Eradicate circles.
decay :: Time -> Set Time -> Set Time
decay t = snd $ Set.split t

phases :: Set Time -- ^ timings
    -> Time -- ^ life span
    -> Time -- ^ the current time
    -> [Float] -- ^ phase
phases s len t = map ((/len) . subtract t) -- transform to an interval [0, 1]
  $ Set.toList
  $ fst $ Set.split (t + len) -- before the limit

renderGame :: Set Time -> Time -> Picture
renderGame ts t = mconcat [color blue $ circles (phases ts 1 t)
    , V2 320 480 `translate` color black (circleOutline 48)]

circles :: [Float] -> Picture
circles = foldMap (\p -> V2 320 ((1 - p) * 480) `translate` circleOutline 48)

allTimings :: Set Time
allTimings = Set.fromList $ concat $ zipWith (\t ch -> [t | ch == '*'])
  (iterate (+(60/160/2)) 1) -- the list of timings where circles may spawn
  "*-*******-*-*-*-" -- asterisk indicates that a circle can spawn

main = runSystemDefault $ do
  time <- new $ variable 0
  timings <- new $ variable allTimings
  linkPicture $ \dt -> do
    t <- time .- get
    time .- put (t + dt)
    ts <- timings .- get
    timings .- put (decay (t - 1) ts)
    return $ renderGame ts t
  stand