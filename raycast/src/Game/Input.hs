module Game.Input (InputState, initialInput, updateInputState, keyPressed, keyReleased, keyDown, keyUp, Key) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List (foldl')

import Engine.RuntimeInterface

type Key = Int
data InputState = InputState { keysPressed :: Set.Set Key, keysReleased :: Set.Set Key, keyState :: Map.Map Key Bool }

initialInput = InputState Set.empty Set.empty Map.empty

keyPressed :: InputState -> Key -> Bool
keyPressed is k = Set.member k $ keysPressed is

keyReleased :: InputState -> Key -> Bool
keyReleased is k = Set.member k $ keysReleased is

keyDown :: InputState -> Key -> Bool
keyDown is k = (Map.member k $ keyState is) && (keyState is Map.! k)

keyUp :: InputState -> Key -> Bool
keyUp is k = not $ keyDown is k

updateInputState :: UpdateFrame -> InputState -> InputState
updateInputState uf is = InputState (Set.fromList $ ufKeysPressed uf) (Set.fromList $ ufKeysReleased uf) withPressedReleased
  where withPressed = insertEvery (keyState is) True $ufKeysPressed uf
        withPressedReleased = insertEvery withPressed False $ ufKeysReleased uf
        insertEvery :: Map.Map Key Bool -> Bool -> [Key] -> Map.Map Key Bool
        insertEvery m v ks = foldl' (\m k -> Map.insert k v m) m ks