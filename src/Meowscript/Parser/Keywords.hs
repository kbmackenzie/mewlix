{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Parser.Keywords
( func
) where

import Data.Text (Text)

func :: Text
func = "=^.x.^="

return :: Text
return = "bring"

end :: Text
end = "leave"

continue :: Text
continue = "rest"

break :: Text
break = "run away"

ifBlock :: Text
ifBlock = "mew?"

elseBlock :: Text
elseBlock = "hiss!"

whileLoop :: Text
whileLoop = "scratch while"
