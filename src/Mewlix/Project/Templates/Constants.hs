module Mewlix.Project.Templates.Constants
(
) where

newtype Template = Template { getTemplate :: FilePath }
    deriving (Eq, Show)

--templateJS :: Language -> Template

