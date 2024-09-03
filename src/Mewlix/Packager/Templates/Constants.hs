{-# LANGUAGE LambdaCase #-}

module Mewlix.Packager.Templates.Constants
( getTemplate
, template
) where

import Mewlix.Packager.Data.Types (ProjectMode(..))
import System.FilePath ((</>))

newtype Template = Template { getTemplate :: FilePath }
    deriving (Eq, Show)

templatePath :: FilePath -> FilePath
templatePath = ("static/templates/" </>)

template :: ProjectMode -> Template
template = \case
    Console -> Template { getTemplate = templatePath "console.zip" }
    Graphic -> Template { getTemplate = templatePath "graphic.zip" }
    Node    -> Template { getTemplate = templatePath "node.zip"    }
