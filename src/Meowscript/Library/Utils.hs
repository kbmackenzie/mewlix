module Meowscript.Library.Utils
( makeIFunc
) where

import Meowscript.Abstract.Atom
import Meowscript.Data.Key (Key)
import qualified Meowscript.Data.Stack as Stack

makeIFunc :: Key -> [Key] -> IFunc -> (Key, MeowAtom)
makeIFunc key arglist f = do
    let params = Stack.fromList arglist
    let arity  = Stack.length params
    let func   = MeowIFunction {
        ifuncName   = key,
        ifuncArity  = arity,
        ifuncParams = params,
        ifunc       = f
    }
    (key, MeowIFunc func)
