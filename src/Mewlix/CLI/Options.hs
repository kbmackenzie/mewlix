module Mewlix.CLI.Options
( runOptions
) where

import Mewlix.Project (ProjectMode(..), Port, defaultMode)
import Options.Applicative

data ProjectOptions = ProjectOptions
    { filesOpt :: [FilePath]
    , nameOpt  :: Maybe String
    , entryOpt :: Maybe FilePath
    , modeOpt  :: Maybe ProjectMode }
    deriving (Show)

data MewlixOptions =
      BuildOpt ProjectOptions
    | RunOpt ProjectOptions Port
    | PackageOpt ProjectOptions
    | CleanOpt
    deriving (Show)

projectOptions :: Parser ProjectOptions
projectOptions = do
    let files :: Parser [FilePath]
        files = many (argument str (metavar "FILES"))

    let name :: Parser String
        name = strOption
             ( long "name"
            <> short 'n'
            <> metavar "STRING"
            <> help "Name of project" )

    let entry :: Parser FilePath
        entry = strOption
             ( long "entrypoint"
            <> short 'e'
            <> metavar "KEY"
            <> help "Project entrypoint" )

    let console :: Parser ProjectMode
        console = flag' Console
             ( long "console"
            <> short 'c'
            <> help "Console frontend" )

    let graphic :: Parser ProjectMode
        graphic = flag' Graphic
             ( long "graphic"
            <> short 'g'
            <> help "Graphic frontend" )

    let library :: Parser ProjectMode
        library = flag' Library
             ( long "library"
            <> short 'l'
            <> help "No frontend" )

    ProjectOptions
        <$> files
        <*> optional name
        <*> optional entry
        <*> optional (console <|> graphic <|> library)

port :: Parser Port
port = option auto
     ( long "port"
    <> short 'p'
    <> help "Project port" )

makeInfo :: Parser a -> String -> ParserInfo a
makeInfo parser desc = info (parser <**> helper) (fullDesc <> progDesc desc)

parseOptions :: Parser MewlixOptions
parseOptions = do
    let build :: Mod CommandFields MewlixOptions
        build = command "build" $ makeInfo parser "Build project"
            where parser = BuildOpt <$> projectOptions

    let run :: Mod CommandFields MewlixOptions
        run = command "run" $ makeInfo parser "Run project in a local server"
            where parser = RunOpt <$> projectOptions <*> port

    let package :: Mod CommandFields MewlixOptions
        package = command "package" $ makeInfo parser "Package a project into a .zip archive"
            where parser = PackageOpt <$> projectOptions

    let clean :: Mod CommandFields MewlixOptions
        clean = command "clean" $ makeInfo parser "Clean project"
            where parser = pure CleanOpt

    subparser (build <> run <> package <> clean)

runOptions :: IO MewlixOptions
runOptions = execParser $ info (parseOptions <**> helper)
     ( fullDesc
    <> progDesc "Mewlix compiler"
    <> header "mewlix - a compiler for a cat-oriented programming language" )
