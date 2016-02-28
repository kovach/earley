{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.String
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (mapMaybe)
import Data.List (intercalate)

type Node = String
type Terminal = Char
data Symbol = N Node | T Terminal
  deriving (Show, Eq, Ord)

data Rule = R
  { left :: Node
  , right :: [Symbol]
  }
  deriving (Show, Eq, Ord)

data Item = I
  { pos :: Int
  , level :: Level
  , rule :: Rule
  , ptrs :: [Item]
  }
  deriving (Show, Eq, Ord)

type State = Set Item
type Level = Int
type World = (Map Level State, [Item])
type Input = String

parse :: Node -> Input -> World
parse nt s = fix (update s) (M.empty, [I 0 0 r [] | r <- rules ! nt])

fix :: (t -> Maybe t) -> t -> t
fix f x =
  case f x of
    Nothing -> x
    Just x' -> fix f x'

update :: Input -> World -> Maybe World
update _ (m, []) = Nothing
update s (m, i : is) =
  case M.lookup (level i) m of
    Nothing -> new
    Just s ->
      if i `S.member` s
      then Just (m, is) -- don't re-process
      else new
  where
    new =
      let items = step s m i in
      Just (M.insertWith S.union (level i) (S.singleton i) m, is ++ items)

getLevel :: [r] -> Int -> Maybe r
getLevel s i | i < length s = Just $ s !! i
getLevel _ _ = Nothing

step :: Input -> Map Int State -> Item -> [Item]
-- subrule is finished
step _ m i@(I pos level (R l []) _) = done (S.toList $ m ! pos) l i
-- if next character matches, proceed
step s m it@(I pos level (R l (T t : is)) p) =
  if getLevel s level == Just t
  then [I pos (level+1) (R l is) p]
  else []
-- try matching subrule
step _ m (I pos level (R l (N n : is)) _) | Just rs <- M.lookup n rules = 
  [I level level r [] | r <- rs]

-- Finds rules containing the nonterminal parsed by `i`
-- Increments their rhs pointer and adds `i` to their `ptrs` list
done :: [Item] -> Node -> Item -> [Item]
done set node i = mapMaybe ok set
  where
    ok (I pos _ (R l (x : xs)) ps) | x == N node = Just
      (I pos (level i) (R l xs) (i : ps))
    ok _ = Nothing

main' nt s = putStrLn . printResult nt s . fst $ parse nt s
main = main' "S" "1+1*1"

rules = ruleMap $
  [ R "S" [N "E"]
  , R "E" [N "+"]
  , R "E" [N "*"]
  , R "E" [N "1"]
  , R "1" [T '1']
  , R "+" [N "E", T '+', N "E"]
  , R "*" [N "E", T '*', N "E"]
  ]

-- Utilities --
printResult nt s m =
  unlines $ map printItem $
  filter (matches nt) $ filter complete $
  S.toList $ m ! (length s)

matches :: Node -> Item -> Bool
matches nt (I _ _ (R l _) _) | l == nt = True
matches _ _ = False

ruleMap rs = foldr (\(k, r) -> M.insertWith (++) k [r]) M.empty (map pair rs)
  where
    pair r@(R l _) = (l, r)

showMap m = "\n" ++ unlines (map showPair (M.toList m))
  where
    showPair (key, items) = unlines $ [show key] ++ map (("  " ++) . show) (S.toList items)

wrap :: String -> String
wrap s = "(" ++ s ++ ")"

complete (I 0 _ (R _ []) _) = True
complete _ = False

printItem :: Item -> String
printItem (I _ _ (R l []) ptrs) = wrap $
  intercalate " " (l : map printItem (reverse ptrs))
