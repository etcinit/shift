{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Shift.Rendering where

import Data.List (sortOn)

import           Control.Lens            ((^.))
import           Control.Monad.Catch     (MonadThrow)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.State     (MonadState)
import           Data.Git                (Commit, Ref, commitAuthor)
import           Data.Monoid             ((<>))
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import qualified Data.Text               as T (take)
import qualified Data.Text.IO            as TIO (putStrLn)
import           Data.Versions           (prettyV)

import Shift.Types

renderRef
  :: (MonadIO m, MonadState s m, ClientState s, MonadThrow m)
  => Ref
  -> m Text
renderRef ref = do
  url <- getRefURL ref

  let shortRef = T.take 7 . cs . show $ ref

  pure $ case url of
    Just url_ -> "[[`" <> shortRef <> "`](" <> url_ <> ")]"
    Nothing -> "[`" <> shortRef <> "`]"

renderConventionalCommit
  :: (MonadIO m, MonadState s m, ClientState s, MonadThrow m)
  => ConventionalGroup
  -> m Text
renderConventionalCommit (ref, commit, pc) = do
  renderedRef <- renderRef ref
  authorText <- renderAuthor commit ref

  pure . mconcat $
    [ "- "
    , renderedRef
    , " "
    , bold $ pc ^. ccScope <> ":"
    , " "
    , pc ^. ccSubject
    , authorText
    ]

renderAuthor
  :: (MonadIO m, MonadState s m, ClientState s, MonadThrow m)
  => Commit
  -> Ref
  -> m Text
renderAuthor commit ref = do
  authorInfo <- getAuthorInfo (commitAuthor commit) ref

  pure $ case authorInfo of
    Just (username, authorUrl) -> " [(" <> username <> ")](" <> authorUrl <> ")"
    Nothing -> ""

renderMiscCommit
  :: (MonadIO m, MonadState s m, ClientState s, MonadThrow m)
  => MiscGroup
  -> m Text
renderMiscCommit (ref, commit, MiscCommit subject) = do
  renderedRef <- renderRef ref
  authorText <- renderAuthor commit ref

  pure $ "- " <> renderedRef <> " " <> subject <> authorText

renderMergeCommit
  :: (MonadIO m, MonadState s m, ClientState s, MonadThrow m)
  => MergeGroup
  -> m Text
renderMergeCommit (ref, commit, MergeCommit subject) = do
  renderedRef <- renderRef ref
  authorText <- renderAuthor commit ref

  pure $ "- " <> renderedRef <> " Merge " <> subject <> authorText

renderRange :: TagRef -> TagRef -> Text
renderRange tx ty = mconcat
  [prettyV . _tVersioning $ tx, " to ", prettyV . _tVersioning $ ty]

bold :: Text -> Text
bold x = "**" <> x <> "**"

indented :: Int -> Text -> Text
indented levels x = foldl (\acc _ -> acc <> "  ") "" [1..levels] <> x

indentedL :: Int -> Text -> Text
indentedL levels x = "\\" <> foldl (\acc _ -> acc <> "-") "" [1..levels] <> x

linePadded :: Text -> Text
linePadded x = "\n" <> x <> "\n"

headerOne :: Text -> Text
headerOne = linePadded . (<>) "# "

headerTwo :: Text -> Text
headerTwo = linePadded . (<>) "## "

headerThree :: Text -> Text
headerThree = linePadded . (<>) "### "

printReport
  :: (MonadIO m, MonadState s m, ClientState s, MonadThrow m)
  => ChangeReport
  -> m ()
printReport report = do
  -- Print conventional commits
  conventionalSection "New features:" crFeatures
  conventionalSection "Fixes:" crFixes
  conventionalSection "Updated documentation:" crDocs
  conventionalSection "Refactors:" crRefactors
  conventionalSection "Updated tests:" crTests
  conventionalSection "Style fixes:" crStyles
  conventionalSection "Chores:" crChores

  -- Print miscs and merges
  miscCommits <- mapM renderMiscCommit (report ^. crMisc)
  mergeCommits <- mapM renderMergeCommit (report ^. crMerges)

  textSection "Miscellaneous changes:" miscCommits
  textSection "Merge commits:" mergeCommits
  where
    conventionalSection label sectionLens = do
      let rawCommits = sortOn (\(_,_,pc) -> pc ^. ccScope) (report ^. sectionLens)

      commits <- mapM renderConventionalCommit rawCommits
      textSection label commits

    textSection label commits = case commits of
      [] -> pure ()
      _ -> do
        liftIO . TIO.putStrLn . headerTwo $ label

        liftIO $ mapM_ (TIO.putStrLn . indented 1) commits

