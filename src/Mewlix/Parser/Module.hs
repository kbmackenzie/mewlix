module Mewlix.Parser.Module
( parseModuleName
) where

import Mewlix.Abstract.Module
import Mewlix.Parser.Utils
import Data.Text (Text)
import qualified Data.List.NonEmpty as NonEmpty
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar

{- Parse module name. Name parts can be reserved keywords. -}
parseModuleName :: Parser ModuleName
parseModuleName = do
    let namePart :: Parser Text
        namePart = Mega.takeWhile1P (Just "module name part") isKeyChar

    let parseName :: Parser [Text]
        parseName = Mega.sepBy1 namePart (MChar.char '.')

    ModuleName . NonEmpty.fromList <$> lexeme parseName <?> "module name"
