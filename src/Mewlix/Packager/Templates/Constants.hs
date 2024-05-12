{-# LANGUAGE LambdaCase #-}

module Mewlix.Packager.Templates.Constants
( getTemplate
, template
) where

import Mewlix.Packager.Maker (Language(..))
import Mewlix.Packager.Data.Types (ProjectMode(..))
import System.FilePath ((</>))

newtype Template = Template { getTemplate :: FilePath }
    deriving (Eq, Show)

templatePath :: FilePath -> FilePath
templatePath = ("templates" </>)

template :: Language -> ProjectMode -> Template
template JavaScript = \case
    Console -> Template { getTemplate = templatePath "js/console.zip" }
    Graphic -> Template { getTemplate = templatePath "js/graphic.zip" }
    Library -> Template { getTemplate = templatePath "js/library.zip" }
