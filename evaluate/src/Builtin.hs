module Builtin where

import qualified Value as V

builtinLength :: V.Value -> V.Value
builtinLength (V.StringValue s) = V.IntegerValue (toInteger (length s))
builtinLength (V.ListValue l) = V.IntegerValue (toInteger (length l))
builtinLength _ = V.NullValue

builtinRead :: (V.Value, V.Value) -> V.Value
builtinRead (V.ListValue l, V.IntegerValue n) = case (get n l) of
                                                    Just x -> x
                                                    Nothing -> V.NullValue
    where get :: Integer -> [a] -> Maybe a
          get n (x:xs) | n > 0 = get (n - 1) xs
                       | n == 0 = Just x
                       | n < 0 = Nothing
          get _ [] = Nothing
builtinRead _ = V.NullValue

builtinModify :: (V.Value, V.Value, V.Value)  -> V.Value
builtinModify (V.ListValue l, V.IntegerValue n, x) = case (set n x l) of
                                                         Just ys -> V.ListValue ys
                                                         Nothing -> V.NullValue
    where set :: Integer -> a -> [a] -> Maybe [a]
          set n x (y:ys) | n > 0 = do ys' <- set (n - 1) x ys
                                      return (y:ys')
                         | n == 0 = Just (x:ys)
                         | n < 0 = Nothing
          set _ _ [] = Nothing
