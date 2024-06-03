module Mewlix.Parser.Keyword
( Keyword(..)
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

class Keyword a where
    keyword :: a -> Parser ()

instance Keyword SimpleKeyword where
    keyword key = (lexeme . Mega.try) $ do
        (void . MChar.string . unwrapKeyword) key
        Mega.notFollowedBy (Mega.satisfy isKeyChar)

instance Keyword LongSymbol where
    keyword = lexeme . void . MChar.string' . unwrapSymbol

instance Keyword WordSequence where
    keyword = Mega.try . mapM_ keyword . unwrapWords

instance (Keyword a) => Keyword [a] where
    keyword = Mega.choice . map keyword

anyKeyword :: (Keyword a) => [a] -> Parser ()
anyKeyword = Mega.choice . map keyword
