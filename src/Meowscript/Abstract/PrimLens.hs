{-# LANGUAGE TemplateHaskell #-}

module Meowscript.Abstract.PrimLens
( unboxStrL
, strLenL
, unboxStackL
, stackLenL
) where

import Meowscript.Abstract.Meow
import Lens.Micro.Platform (makeLensesFor)

$(makeLensesFor
    [ ("unboxStr"  , "unboxStrL" )
    , ("strLen"    , "strLenL"   ) ] ''BoxedString)

$(makeLensesFor
    [ ("unboxStack" , "unboxStackL")
    , ("stackLen"   , "stackLenL"  ) ] ''BoxedStack)
