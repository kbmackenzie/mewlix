{-# LANGUAGE TemplateHaskell #-}

module Meowscript.Abstract.PrimLens
( unboxStrL
, strLenL
, unboxListL
, listLenL
) where

import Meowscript.Abstract.Atom
import Lens.Micro.Platform (makeLensesFor)

$(makeLensesFor
    [ ("unboxStr"  , "unboxStrL" )
    , ("strLen"    , "strLenL"   ) ] ''BoxedString)

$(makeLensesFor
    [ ("unboxList" , "unboxListL")
    , ("listLen"   , "listLenL"  ) ] ''BoxedList)
