module Mewlix.CLI.Options
( ProjectOptions(..)
, FlagOptions(..)
, MewlixOptions(..)
, getOptions
) where

import Mewlix.Packager (ProjectMode(..), Port(..))
import Options.Applicative
    ( Parser
    , ParserInfo
    , CommandFields
    , Mod
    , help
    , long
    , short
    , metavar
    , switch
    , flag'
    , str
    , auto
    , (<|>)
    , (<**>)
    , option
    , optional
    , many
    , argument
    , strOption
    , command
    , subparser
    , fullDesc
    , progDesc
    , info
    , helper
    , execParser
    , header
    )

data ProjectOptions = ProjectOptions
    { filesOpt  :: [FilePath]
    , nameOpt   :: Maybe String
    , entryOpt  :: Maybe FilePath
    , modeOpt   :: Maybe ProjectMode
    , assetsOpt :: [FilePath] }
    deriving (Show)

data FlagOptions = FlagOptions
    { quietFlag    :: Bool
    , prettyFlag   :: Bool
    , noStdFlag    :: Bool
    , noReadMeFlag :: Bool }
    deriving (Show)

data MewlixOptions =
      BuildOpt      ProjectOptions FlagOptions Bool
    | RunOpt        ProjectOptions FlagOptions Bool (Maybe Port) Bool
    | PackageOpt    ProjectOptions FlagOptions Bool
    | NewOpt        (Maybe String) (Maybe ProjectMode)
    | CleanOpt      Bool
    deriving (Show)

projectMode :: Parser ProjectMode
projectMode = console <|> graphic <|> library
    where
        console :: Parser ProjectMode
        console = flag' Console
             ( long "console"
            <> short 'c'
            <> help "Console template" )

        graphic :: Parser ProjectMode
        graphic = flag' Graphic
             ( long "graphic"
            <> short 'g'
            <> help "Graphic template" )

        library :: Parser ProjectMode
        library = flag' Library
             ( long "library"
            <> short 'l'
            <> help "Library template" )

flagOptions :: Parser FlagOptions
flagOptions = FlagOptions <$> quiet <*> pretty <*> noStd <*> noReadMe
    where
        quiet :: Parser Bool
        quiet = switch
             ( long "quiet"
            <> short 'q'
            <> help "Silence compiler messages" )

        pretty = switch
             ( long "pretty"
            <> short 'p'
            <> help "Prettify compiler output" )

        noStd :: Parser Bool
        noStd = switch
             ( long "no-std"
            <> help "Do not include std library binding when compiling" )

        noReadMe :: Parser Bool
        noReadMe = switch
             ( long "no-readme"
            <> help "Do not auto-generate a README file" )

projectOptions :: Parser ProjectOptions
projectOptions = options
    where
        options = ProjectOptions
            <$> files
            <*> optional name
            <*> optional entry
            <*> optional projectMode
            <*> many asset
        
        files :: Parser [FilePath]
        files = many (argument str (metavar "FILES"))

        name :: Parser String
        name = strOption
             ( long "name"
            <> short 'n'
            <> metavar "STRING"
            <> help "Project name" )

        entry :: Parser FilePath
        entry = strOption
             ( long "entrypoint"
            <> short 'e'
            <> metavar "KEY"
            <> help "Project entrypoint" )

        asset :: Parser FilePath
        asset = strOption
             ( long "asset"
            <> short 'a'
            <> metavar "PATH"
            <> help "Project asset" )

port :: Parser Port
port = Port <$> option auto
     ( long "port"
    <> metavar "INT"
    <> help "Port number to use when running project" )

standalone :: Parser Bool
standalone = switch
     ( long "standalone"
    <> short 's'
    <> help "Ignore project file, use project defaults" )

noBrowser :: Parser Bool
noBrowser = switch
     ( long "no-browser"
    <> help "Don't launch web browser when running project" )

makeInfo :: Parser a -> String -> ParserInfo a
makeInfo parser desc = info (parser <**> helper) (fullDesc <> progDesc desc)

parseOptions :: Parser MewlixOptions
parseOptions = options
    where
        options = subparser (build <> run <> package <> new <> clean)

        build :: Mod CommandFields MewlixOptions
        build = command "build" $ makeInfo parser "Build project"
            where parser = BuildOpt
                    <$> projectOptions
                    <*> flagOptions
                    <*> standalone

        run :: Mod CommandFields MewlixOptions
        run = command "run" $ makeInfo parser "Run project"
            where parser = RunOpt
                    <$> projectOptions
                    <*> flagOptions
                    <*> standalone
                    <*> optional port
                    <*> noBrowser

        package :: Mod CommandFields MewlixOptions
        package = command "package" $ makeInfo parser "Package project's build output into a .zip archive"
            where parser = PackageOpt
                    <$> projectOptions
                    <*> flagOptions
                    <*> standalone

        new :: Mod CommandFields MewlixOptions
        new = command "new" $ makeInfo parser "Create a new project in the current directory"
            where parser = NewOpt
                    <$> optional (argument str (metavar "NAME"))
                    <*> optional projectMode

        clean :: Mod CommandFields MewlixOptions
        clean = command "clean" $ makeInfo parser "Clean project directory, removing build folder"
            where parser = CleanOpt <$> standalone

getOptions :: IO MewlixOptions
getOptions = execParser $ info (parseOptions <**> helper)
     ( fullDesc
    <> header "mewlix - a compiler for a cat-oriented programming language" )
