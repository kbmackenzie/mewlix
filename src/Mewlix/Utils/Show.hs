module Mewlix.Utils.Show
( showT
) where

import qualified Data.Text as Text

showT :: (Show a) => a -> Text.Text
showT = Text.pack . show
