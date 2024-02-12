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
template Javascript = \case
    Console -> Template { getTemplate = templatePath "js/console" }
    Graphic -> Template { getTemplate = templatePath "js/graphic" }
    Library -> Template { getTemplate = templatePath "js/library" }
