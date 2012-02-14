module Higher where

orP :: [a -> Bool] -> (a -> Bool)
orP preds a = any ($ a) preds

andP :: [a -> Bool] -> (a -> Bool)
andP preds a = all ($ a) preds

notP :: (a -> Bool) -> a -> Bool
notP p x = not $ p x
