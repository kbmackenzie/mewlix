{-# LANGUAGE TemplateHaskell #-}

module Mewlix.Abstract.PrimLens
( unboxStrL
, strLenL
, unboxStackL
, stackLenL
, funcNameL
, funcArityL
, funcParamsL
, funcBodyL
, funcClosureL
, methodOwnerL
, methodFuncL
) where

import Mewlix.Abstract.Meow
import Lens.Micro.Platform (makeLensesFor)

$(makeLensesFor
    [ ("unboxStr"       , "unboxStrL"       )
    , ("strLen"         , "strLenL"         ) ] ''BoxedString)

$(makeLensesFor
    [ ("unboxStack"     , "unboxStackL"     )
    , ("stackLen"       , "stackLenL"       ) ] ''BoxedStack)

$(makeLensesFor
    [ ("funcName"       , "funcNameL"       )
    , ("funcArity"      , "funcArityL"      )
    , ("funcParams"     , "funcParamsL"     ) 
    , ("funcBody"       , "funcBodyL"       )
    , ("funcClosure"    , "funcClosureL"    ) ] ''MeowFunction)

$(makeLensesFor
    [ ("methodOwner"    , "methodOwnerL"    )
    , ("methodFunc"     , "methodFuncL"     ) ] ''MeowMethod)
