{-# LANGUAGE StrictData #-}

module Mewlix.CLI.Options
( ProjectOptions(..)
, BuildFlags(..)
, MewlixAction(..)
, MewlixCommand(..)
, RunOptions(..)
, getCommand
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

data BuildFlags = BuildFlags
    { prettyFlag   :: Bool
    , noStdFlag    :: Bool
    , noReadMeFlag :: Bool }
    deriving (Show)

data RunOptions = RunOptions
    { runPort       :: Maybe Port
    , runRebuild    :: Bool
    , runNoBrowser  :: Bool      }
    deriving (Show)

data MewlixAction =
      BuildAction    ProjectOptions BuildFlags
    | RunAction      ProjectOptions BuildFlags RunOptions
    | PackageAction  ProjectOptions BuildFlags
    | NewAction      (Maybe String) (Maybe ProjectMode)
    | CleanAction
    deriving (Show)

data MewlixCommand = MewlixCommand
    { commandAction     :: MewlixAction
    , commandQuiet      :: Bool
    , commandStandalone :: Bool }
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
        library = flag' Node
             ( long "node"
            <> short 'n'
            <> help "Node.js template" )

quiet :: Parser Bool
quiet = switch
     ( long "quiet"
    <> short 'q'
    <> help "Silence compiler messages" )

standalone :: Parser Bool
standalone = switch
     ( long "standalone"
    <> short 's'
    <> help "Ignore project file, use project defaults" )

buildFlags :: Parser BuildFlags
buildFlags = BuildFlags <$> pretty <*> noStd <*> noReadMe
    where
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
            <> short 'o'
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

noBrowser :: Parser Bool
noBrowser = switch
     ( long "no-browser"
    <> help "Don't launch web browser when running project" )

rebuild :: Parser Bool
rebuild = switch
     ( long "rebuild"
    <> short 'r'
    <> help "Rebuild project" )

runOptions :: Parser RunOptions
runOptions = RunOptions
    <$> optional port
    <*> rebuild
    <*> noBrowser

makeInfo :: Parser a -> String -> ParserInfo a
makeInfo parser desc = info (parser <**> helper) (fullDesc <> progDesc desc)

action :: Parser MewlixAction
action = subparser actions
    where
        actions :: Mod CommandFields MewlixAction
        actions = mconcat [init_, build, run, package, clean]

        build :: Mod CommandFields MewlixAction
        build = command "build" $ makeInfo parser "Build project"
            where parser = BuildAction <$> projectOptions <*> buildFlags

        run :: Mod CommandFields MewlixAction
        run = command "run" $ makeInfo parser "Run project"
            where parser = RunAction <$> projectOptions <*> buildFlags <*> runOptions

        package :: Mod CommandFields MewlixAction
        package = command "package" $ makeInfo parser "Package project's build output into a .zip archive"
            where parser = PackageAction <$> projectOptions <*> buildFlags

        init_ :: Mod CommandFields MewlixAction
        init_ = command "init" $ makeInfo parser "Create a new project in the current directory"
            where parser = NewAction
                    <$> optional (argument str (metavar "NAME"))
                    <*> optional projectMode

        clean :: Mod CommandFields MewlixAction
        clean = command "clean" $ makeInfo parser "Clean project directory, removing build folder"
            where parser = pure CleanAction

parseAll :: Parser MewlixCommand
parseAll = MewlixCommand <$> action <*> quiet <*> standalone

getCommand :: IO MewlixCommand
getCommand = execParser $ info (parseAll <**> helper)
     ( fullDesc
    <> header "mewlix - a compiler for a cat-oriented programming language" )
