{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Shift.CLI where

import Options.Applicative

import Control.Lens (makeLenses)

data ShiftOptions = ShiftOptions
  { _soCommand          :: ShiftCommand
  , _soRepositoryPath   :: Maybe String
  , _soHostingType      :: HostingType
  , _soGitHubOwner      :: Maybe String
  , _soGitHubRepository :: Maybe String
  , _soGitHubToken      :: Maybe String
  } deriving (Show, Eq)

data ShiftCommand = GenerateCommand deriving (Show, Eq, Enum)

data HostingType = GitHubType | GitType deriving (Show, Eq, Enum)

shiftOptions :: Parser ShiftOptions
shiftOptions = ShiftOptions
  <$> shiftCommand
  <*> optional (strOption
    ( long "repository-path"
    <> metavar "PATH"
    <> help "Path where the Git repository is located (Default: .git)"
    ))
  <*> option hostingType
    ( long "hosting-type"
    <> short 't'
    <> metavar "TYPE"
    <> help "Which kind of service the repository is hosted on"
    )
  <*> optional (strOption
    ( long "github-owner"
    <> metavar "USERNAME"
    <> help "Username who owns the repository"
    ))
  <*> optional (strOption
    ( long "github-repository"
    <> metavar "REPOSITORY"
    <> help "Name of the repository"
    ))
  <*> optional (strOption
    ( long "github-token"
    <> metavar "TOKEN"
    <> help "GitHub access token"
    ))

shiftCommand :: Parser ShiftCommand
shiftCommand = subparser $
  command "generate"
    (info (pure GenerateCommand)
      (progDesc "Generate changelog")
    )

hostingType :: ReadM HostingType
hostingType = eitherReader $ \case
  "github" -> Right GitHubType
  "git" -> Right GitType
  x -> Left $ "`" ++ x ++ "` is not a supported hosting type"

makeLenses ''ShiftOptions
