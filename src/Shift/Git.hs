{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Shift.Git where

import Control.Monad  (void)
import Data.Either    (rights)
import Data.List      (sortBy)
import Data.Maybe     (catMaybes)
import Data.Set       as S (toList)
import Data.Tuple     (swap)
import System.Process

import           Control.Lens            ((^.))
import           Control.Monad.Catch     (throwM)
import           Control.Monad.Reader    (runReaderT)
import           Control.Monad.State     (runStateT)
import           Control.Monad.Trans     (liftIO)
import           Data.ByteString.Char8   (ByteString)
import qualified Data.ByteString.Char8   as BS
import           Data.Default            (def)
import           Data.Git
import           Data.Git.Ref            (fromHex, isHex)
import           Data.Git.Storage.Object
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as TIO
import           Data.Versions           (parseV)
import           GitHub.Auth             (Auth (OAuth))
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Text.Megaparsec         (ParseError)

import Shift.CLI
import Shift.Processing
import Shift.Rendering
import Shift.Types
import Shift.Utilities  (orError, pairs)

parseTag :: RefName -> Either ParseError TagRef
parseTag ref = case parseV . cs . refNameRaw $ ref of
  Left e -> Left e
  Right v -> Right (TagRef ref v)

tempMain :: ShiftOptions -> IO ()
tempMain opts = withRepo ".git" $ \repo -> do
  tags <- tagList repo

  let sortedVersions = sortBy (flip compare) . rights $ parseTag <$> toList tags
      pairedTags = swap <$> pairs sortedVersions

  case opts ^. soHostingType of
    GitHubType -> do
      state <- initGitHubState

      runReaderT
        (void $ runStateT (mapM_ (renderDiff repo) pairedTags) state)
        opts

  where
    initGitHubState = do
      manager <- newManager tlsManagerSettings

      token <- (opts ^. soGitHubToken) `orError` SEMissingGitHubToken
      repositoryOwner <- (opts ^. soGitHubOwner) `orError` SEMissingGitHubOwner
      repositoryName <- (opts ^. soGitHubRepository)
        `orError` SEMissingGitHubRepository

      pure GitHubClientState
        { _gcsCache = def
        , _gcsAuth = OAuth (cs token)
        , _gcsManager = manager
        , _gcsOwner = cs repositoryOwner
        , _gcsRepository = cs repositoryName
        }

renderDiff :: Git -> (TagRef, TagRef) -> GitM ()
renderDiff repo (tx, ty) = do
  liftIO . TIO.putStrLn . headerOne $ renderRange tx ty

  diff <- lookupCommitsDiff repo tx ty

  case diff of
    [] -> throwM SEUnableToComputeDiff
    diff_ -> printReport (generateReport . rights $ parseCommit <$> diff_)

lookupCommitsDiff :: Git -> TagRef -> TagRef -> GitM [(Ref, Commit)]
lookupCommitsDiff repo x y = do
  rawOutput <- liftIO $ readCreateProcess (shell gitCommand) ""

  catMaybes <$> mapM
    (lookupRawRef repo)
    (filter (not . T.null) . T.splitOn "\n" . cs $ rawOutput)

  where
    gitCommand = mconcat
      [ "git rev-list "
      , refNameRaw . _tRef $ x
      , "..."
      , refNameRaw . _tRef $ y
      ]

lookupRawRef :: Git -> Text -> GitM (Maybe (Ref, Commit))
lookupRawRef repo rr
  = if isHex . cs $ rr
    then do
        let ref = fromHex . cs $ rr

        object <- liftIO $ getObject repo ref True

        case object of
          Just (ObjCommit commit) -> pure $ Just (ref, commit)
          _ -> pure Nothing
    else pure Nothing

commitSummary :: Commit -> ByteString
commitSummary = head . BS.split '\n' . commitMessage
