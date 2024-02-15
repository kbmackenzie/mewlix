module Mewlix.CLI.Options
( ProjectOptions(..)
, FlagOptions(..)
, MewlixOptions(..)
, getOptions
) where

import Mewlix.Project (ProjectMode(..), Port)
import Options.Applicative

data ProjectOptions = ProjectOptions
    { filesOpt :: [FilePath]
    , nameOpt  :: Maybe String
    , entryOpt :: Maybe FilePath
    , modeOpt  :: Maybe ProjectMode     }
    deriving (Show)

data FlagOptions = FlagOptions
    { quietFlag  :: Bool
    , noStdFlag  :: Bool  }
    deriving (Show)

data MewlixOptions =
      BuildOpt      ProjectOptions FlagOptions Bool
    | RunOpt        ProjectOptions FlagOptions Bool (Maybe Port)
    | PackageOpt    ProjectOptions FlagOptions Bool
    | CleanOpt
    deriving (Show)

projectMode :: Parser ProjectMode
projectMode = console <|> graphic <|> library
    where
        console :: Parser ProjectMode
        console = flag' Console
             ( long "console"
            <> short 'c'
            <> help "Console frontend" )

        graphic :: Parser ProjectMode
        graphic = flag' Graphic
             ( long "graphic"
            <> short 'g'
            <> help "Graphic frontend" )

        library :: Parser ProjectMode
        library = flag' Library
             ( long "library"
            <> short 'l'
            <> help "No frontend" )

flagOptions :: Parser FlagOptions
flagOptions = FlagOptions <$> quiet <*> noStd
    where
        quiet :: Parser Bool
        quiet = switch
             ( long "quiet"
            <> short 'q'
            <> help "Silence console messages" )

        noStd :: Parser Bool
        noStd = switch
             ( long "no-std"
            <> help "Do not include std library binding when compiling" )

projectOptions :: Parser ProjectOptions
projectOptions = options
    where
        options = ProjectOptions
            <$> files
            <*> optional name
            <*> optional entry
            <*> optional projectMode
        
        files :: Parser [FilePath]
        files = many (argument str (metavar "FILES"))

        name :: Parser String
        name = strOption
             ( long "name"
            <> short 'n'
            <> metavar "STRING"
            <> help "Name of project" )

        entry :: Parser FilePath
        entry = strOption
             ( long "entrypoint"
            <> short 'e'
            <> metavar "KEY"
            <> help "Project entrypoint" )

port :: Parser Port
port = option auto
     ( long "port"
    <> short 'p'
    <> help "Project port" )

standalone :: Parser Bool
standalone = switch
     ( long "standalone"
    <> short 's'
    <> help "Ignore project file, use project defaults" )

makeInfo :: Parser a -> String -> ParserInfo a
makeInfo parser desc = info (parser <**> helper) (fullDesc <> progDesc desc)

parseOptions :: Parser MewlixOptions
parseOptions = options
    where
        options = subparser (build <> run <> package <> clean)

        build :: Mod CommandFields MewlixOptions
        build = command "build" $ makeInfo parser "Build project"
            where parser = BuildOpt
                    <$> projectOptions
                    <*> flagOptions
                    <*> standalone

        run :: Mod CommandFields MewlixOptions
        run = command "run" $ makeInfo parser "Run project in a local server"
            where parser = RunOpt
                    <$> projectOptions
                    <*> flagOptions
                    <*> standalone
                    <*> optional port

        package :: Mod CommandFields MewlixOptions
        package = command "package" $ makeInfo parser "Package a project into a .zip archive"
            where parser = PackageOpt
                    <$> projectOptions
                    <*> flagOptions
                    <*> standalone

        clean :: Mod CommandFields MewlixOptions
        clean = command "clean" $ makeInfo parser "Clean project"
            where parser = pure CleanOpt

getOptions :: IO MewlixOptions
getOptions = execParser $ info (parseOptions <**> helper)
     ( fullDesc
    <> header "mewlix - a compiler for a cat-oriented programming language" )
