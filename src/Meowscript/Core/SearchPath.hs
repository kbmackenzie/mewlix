{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.Core.SearchPath
(
) where

import Meowscript.Core.AST
import Meowscript.Parser.Core
import qualified Data.Text as Text
import Meowscript.Utils.IO

stdFiles :: [Text.Text]
stdFiles = 
    [ "std.meows"
    , "io.meows" ]

asStd :: Text.Text -> Text.Text
asStd = Text.append "std/"

asAppDir :: Text.Text -> IO (Either Text.Text Text.Text)
asAppDir path = safeCurrentDir >>= \case
    (Left exception) -> (return . Left) exception
    (Right directory) -> (return . Right) (Text.append directory path)


{-
findFile :: FilePath -> IO (Either Text.Text Text.Text)
findFile path = if (Text.pack path) `elem` stdFiles
    then 
    -}

{-safeReadFile path >>= \case
    (Left exception) -> (return . Left) exception
    (Right contents) -> 

 -}
