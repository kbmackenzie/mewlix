{-# LANGUAGE TemplateHaskell #-}

module Meowscript.Bytecode.PrimLens
( unboxStrL
, strLenL
, unboxListL
, listLenL
) where

import Meowscript.Bytecode.Prim
import Lens.Micro.Platform (makeLensesFor)

$(makeLensesFor
    [ ("unboxStr"  , "unboxStrL" )
    , ("strLen"    , "strLenL"   ) ] ''BoxedString)

$(makeLensesFor
    [ ("unboxList" , "unboxListL")
    , ("listLen"   , "listLenL"  ) ] ''BoxedList)
