module Raycast.SimpleLens (SimpleLens, mkSimpleLens, view, over, set, setIn, mapOverMinLensM, minLensWithValueM, minLensM) where

import Data.Maybe
import Data.Ord
import Control.Applicative

data SimpleLens s t = SL (s -> t) (s -> t -> s)
mkSimpleLens :: (s -> t) -> (s -> t -> s) -> SimpleLens s t
mkSimpleLens view' set' = SL view' set'

view :: SimpleLens s t -> s -> t
view (SL view' _) t = view' t

over :: SimpleLens s t -> s -> (t -> t) -> s
over (SL view' set') s f = set' s (f . view' $ s)

set :: SimpleLens s t -> s -> t -> s
set lens s t = over lens s (const t)

setIn :: SimpleLens s t -> t -> s -> s
setIn lens t s = over lens s (const t)

mapOverMinLensM :: (t -> t -> Ordering) -> (t -> t) -> SimpleLens s (Maybe t) -> SimpleLens s (Maybe t) -> s -> s
mapOverMinLensM c f x y r = fromMaybe r . fmap (\t -> over t r (fmap f)) $ minLensM c x y r

compareWithMinValue f a b = case f a b of GT -> (GT, b)
                                          _  -> (LT, a)

minLensWithValueM :: (t -> t -> Ordering) -> SimpleLens s (Maybe t) -> SimpleLens s (Maybe t) -> s -> Maybe (SimpleLens s (Maybe t), t)
minLensWithValueM f a b s = do
  let a' = view a s
  let b' = view b s
  ordVal <- (compareWithMinValue f <$> a' <*> b') <|> fmap ((,) LT) a' <|> fmap ((,) GT) b'
  case ordVal of (GT, val) -> return (b, val)
                 (_,  val) -> return (a, val)                                  
                                  
minLensM f a b s = fmap fst $ minLensWithValueM f a b s