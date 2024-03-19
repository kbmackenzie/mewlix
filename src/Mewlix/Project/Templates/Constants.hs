{-# LANGUAGE LambdaCase #-}

module Mewlix.Project.Templates.Constants
( getTemplate
, template
) where

import Mewlix.Project.Maker (Language(..))
import Mewlix.Project.Data.Types (ProjectMode(..))
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
