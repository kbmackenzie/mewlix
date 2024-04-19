module Mewlix.Parser.Keyword
( ParseKeyword(..)
, anyKeyword
) where

import Mewlix.Parser.Type (Parser)
import Mewlix.Keywords.Types
    ( SimpleKeyword(..)
    , LongSymbol(..)
    , WordSequence(..)
    )
import Mewlix.Parser.Utils (lexeme, isKeyChar)
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

anyKeyword :: (ParseKeyword a) => [a] -> Parser ()
anyKeyword = Mega.choice . map keyword
