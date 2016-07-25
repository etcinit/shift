{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Shift.Types where

import Control.Exception (Exception, throwIO)
import Data.Monoid       ((<>))
import Prelude           hiding (head, lookup)

import           Control.Lens            (assign, makeClassy, makeLenses, view,
                                          (^.))
import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.Reader    (ReaderT)
import           Control.Monad.State     (MonadState, StateT, gets)
import           Control.Monad.Trans     (liftIO)
import           Data.Default            (Default, def)
import           Data.Git                (Commit, Person, Ref, RefName,
                                          personEmail, personName)
import           Data.HashMap.Strict     (HashMap, insert, lookup)
import           Data.HashSet            (HashSet)
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Vector             as V
import           Data.Versions           (Versioning)
import           GitHub                  (executeRequestWithMgr)
import           GitHub.Auth             (Auth)
import           GitHub.Data.Search      (searchResultResults,
                                          searchResultTotalCount)
import           Network.HTTP.Client     (Manager)

import GitHub.UserSearch
import Shift.CLI         (ShiftOptions)

class ClientState s where
  getRefURL
    :: (MonadIO m, MonadState s m)
    => Ref
    -> m (Maybe Text)
  getAuthorInfo
    :: (MonadIO m, MonadState s m)
    => Person
    -> m (Maybe (Text, Text))

type GitM a = forall s. ClientState s => StateT s (ReaderT ShiftOptions IO) a

data ShiftException
  = SEUnableToComputeDiff
  | SEUnableToLookup String
  | SEMissingGitHubToken
  | SEMissingGitHubOwner
  | SEMissingGitHubRepository
  deriving (Show)

instance Exception ShiftException

type TicketChange = (Text, Text)

data MergeCommit = MergeCommit Text deriving (Show)

data MiscCommit = MiscCommit Text deriving (Show)

data CommitType
  = CTFeature
  | CTFix
  | CTDocs
  | CTStyle
  | CTRefactor
  | CTTest
  | CTChore
  deriving (Show)

data BreakingChange = BreakingChange
  { _bcSubject :: Text
  , _bcBody    :: Text
  } deriving (Show)

data ConventionalCommit = ConventionalCommit
  { _ccType            :: CommitType
  , _ccScope           :: Text
  , _ccSubject         :: Text
  , _ccBody            :: Text
  , _ccBreakingChanges :: [BreakingChange]
  , _ccAffectedTickets :: [HashSet TicketChange]
  } deriving (Show)

data TagRef = TagRef
  { _tRef        :: RefName
  , _tVersioning :: Versioning
  } deriving (Show, Eq)

instance Ord TagRef where
  compare x y = compare (_tVersioning x) (_tVersioning y)

data ParsedCommit
  = PCConventional ConventionalCommit
  | PCMerge MergeCommit
  | PCMisc MiscCommit
  deriving (Show)

type ParsedGroup = (Ref, Commit, ParsedCommit)
type ConventionalGroup = (Ref, Commit, ConventionalCommit)
type MergeGroup = (Ref, Commit, MergeCommit)
type MiscGroup = (Ref, Commit, MiscCommit)

data ChangeReport = ChangeReport
  { _crFeatures        :: [ConventionalGroup]
  , _crFixes           :: [ConventionalGroup]
  , _crDocs            :: [ConventionalGroup]
  , _crStyles          :: [ConventionalGroup]
  , _crRefactors       :: [ConventionalGroup]
  , _crTests           :: [ConventionalGroup]
  , _crChores          :: [ConventionalGroup]
  , _crMerges          :: [MergeGroup]
  , _crMisc            :: [MiscGroup]
  , _crBreakingChanges :: [BreakingChange]
  , _crAffectedTickets :: HashSet TicketChange
  } deriving (Show)

instance Default ChangeReport where
  def = ChangeReport
    { _crFeatures = []
    , _crFixes = []
    , _crDocs = []
    , _crStyles = []
    , _crRefactors = []
    , _crTests = []
    , _crChores = []
    , _crMerges = []
    , _crMisc = []
    , _crBreakingChanges = []
    , _crAffectedTickets = mempty
    }

data RepositoryCache = RepositoryCache
  { _rcRefURLs     :: HashMap Text (Maybe Text)
  , _rcAuthorInfos :: HashMap Text (Maybe (Text, Text))
  }

instance Default RepositoryCache where
  def = RepositoryCache mempty mempty

makeClassy ''RepositoryCache

data GitClientState = GitClientState

instance ClientState GitClientState where
  getRefURL _ = pure Nothing
  getAuthorInfo person = pure . Just $
    ( cs $ personName person
    , mconcat ["mailto://", cs $ personEmail person]
    )

data GitHubClientState = GitHubClientState
  { _gcsCache      :: RepositoryCache
  , _gcsAuth       :: Auth
  , _gcsManager    :: Manager
  , _gcsOwner      :: Text
  , _gcsRepository :: Text
  }

makeLenses ''GitHubClientState

instance ClientState GitHubClientState where
  getRefURL ref = do
    owner <- gets (view gcsOwner)
    repositoryName <- gets (view gcsRepository)

    pure . Just . mconcat $
      [ "https://github.com/"
      , owner
      , "/"
      , repositoryName
      , "/commit/"
      , cs . show $ ref
      ]

  getAuthorInfo person = do
    let email = cs $ personEmail person

    cache <- gets (view $ gcsCache . rcAuthorInfos)

    case lookup email cache of
      Just hit -> pure hit
      Nothing -> do
        manager <- gets (view gcsManager)
        auth <- gets (view gcsAuth)

        results <- liftIO . executeRequestWithMgr manager auth
          $ searchUsersR email

        result <- case results of
          Left e -> liftIO $ throwIO e
          Right results_ -> if (searchResultTotalCount results_ == 1)
            then liftIO $ do
              let user = V.head $ searchResultResults results_
              --print results_
              pure $ Just (user ^. urLogin, user ^. urHtmlUrl)
            else pure Nothing

        assign (gcsCache . rcAuthorInfos) (insert email result cache)

        pure result

makeLenses ''ConventionalCommit
makeLenses ''ChangeReport
makeLenses ''BreakingChange

