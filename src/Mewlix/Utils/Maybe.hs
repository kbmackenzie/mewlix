module Mewlix.Utils.Maybe
( hush
) where

hush :: Either a b -> Maybe b
hush (Right x) = Just x
hush (Left  _) = Nothing
