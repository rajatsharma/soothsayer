module Soothsayer where

import Data.List (intercalate, isPrefixOf)

startswith :: Eq a => [a] -> [a] -> Bool
startswith = isPrefixOf

spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([], [])
spanList func list@(x : xs) =
  if func list
    then (x : ys, zs)
    else ([], list)
  where
    (ys, zs) = spanList func xs

breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
  let (firstline, remainder) = breakList (startswith delim) str
   in firstline : case remainder of
        [] -> []
        x ->
          if x == delim
            then [[]]
            else split delim (drop (length delim) x)

join :: [a] -> [[a]] -> [a]
join = intercalate

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = join new . split old

format :: String -> [String] -> String
format a b = doFormat a (0 :: Int, b)
  where
    doFormat a (_, []) = a
    doFormat a (n, b : bs) = replace (old n) b a `doFormat` (n + 1, bs)
    old n = "{" ++ show n ++ "}"

(***) :: String -> [String] -> String
(***) = format
