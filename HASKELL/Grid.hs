module Grid where
 
import Data.Map (Map)
import qualified Data.Map as Map
 
data Grid p a = Grid { getGrid :: Map p [a] } deriving (Show)
 
fromList :: Ord p => [a] -> (a -> p) -> Grid p a
fromList xs f = Grid . Map.fromListWith (++) $ map (\x -> (f x, [x])) xs
 
at :: Ord p => p -> Grid p a -> [a]
at p = Map.findWithDefault [] p . getGrid
 
keys :: Grid p a -> [p]
keys = Map.keys . getGrid
 
values :: Grid p a -> [a]
values = concat . Map.elems . getGrid
 
update :: Ord p => (p -> [p]) -> ([a] -> a -> a) -> Grid p a -> Grid p a
update f h g = Grid $ Map.mapWithKey k (getGrid g)
  where
    k p = map (h (neighbours p f g))
 
-- =========================================================
 
neighbours :: Ord p => p -> (p -> [p]) -> Grid p a -> [a]
neighbours p f g = concatMap (\i -> at i g) (f p)