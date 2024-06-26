module Mewlix.Parser.Module
( parseModuleKey
) where

import Mewlix.Abstract.Module (ModuleKey(..))
import Mewlix.Parser.Type (Parser)
import Mewlix.Parser.Utils (lexeme, isKeyChar)
import Data.Text (Text)
import qualified Data.List.NonEmpty as NonEmpty
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar

{- Parse module name. Name parts can be reserved keywords. -}
parseModuleKey :: Parser ModuleKey
parseModuleKey = do
    let namePart :: Parser Text
        namePart = Mega.takeWhile1P (Just "module name part") isKeyChar

    let parseKey :: Parser [Text]
        parseKey = Mega.sepBy1 namePart (MChar.char '.')

    ModuleKey . NonEmpty.fromList <$> lexeme parseKey <?> "module name"
