module Util.Util (makeFracPositive, positiveFractionOf, normalizeSafe) where

import Data.Vect.Double.Base

makeFracPositive :: Double -> Double
makeFracPositive x | x < 0 = x + 1
                   | otherwise = x

positiveFractionOf :: Double -> Double                   
positiveFractionOf x = makeFracPositive . snd . properFraction $ x

normalizeSafe :: Vec2 -> Vec2
normalizeSafe v | norm v > 0.0001 = normalize v
                | otherwise = v