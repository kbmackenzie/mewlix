module Mewlix.Parser.Keyword
( ParseKeyword(..)
) where

import Mewlix.Keywords.Types
    ( SimpleKeyword(..)
    , LongSymbol(..)
    , WordSequence(..)
    )
import Mewlix.Parser.Utils
    ( Parser
    , lexeme
    , isKeyChar
    )
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import Control.Monad (void)

class ParseKeyword a where
    keyword :: a -> Parser ()

instance ParseKeyword SimpleKeyword where
    keyword key = (lexeme . Mega.try) $ do
        (void . MChar.string . unwrapKeyword) key
        Mega.notFollowedBy (Mega.satisfy isKeyChar)

instance ParseKeyword LongSymbol where
    keyword = lexeme . void . MChar.string' . unwrapSymbol

instance ParseKeyword WordSequence where
    keyword = Mega.try . mapM_ keyword . unwrapWords
