exists :: Eq a => [a] -> a -> Bool
exists xs x
  | head xs == x = True
  | tail xs == [] = False
  | otherwise = exists (tail xs) x
find :: Eq t => [t] -> (t -> Bool) -> Maybe t
find xs f
  | f(head xs) = Just(head xs)
  | tail xs == [] = Nothing
  | otherwise = find (tail xs) f

