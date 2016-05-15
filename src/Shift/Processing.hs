module Shift.Processing where

import Control.Lens ((<>~), (^.))
import Shift.Types
import Data.Default (def)
import Text.Megaparsec (runParser, ParseError)
import Data.Git (commitMessage, Ref, Commit)
import Data.String.Conversions (cs)

import Shift.Parsers

parseCommit :: (Ref, Commit) -> Either ParseError ParsedGroup
parseCommit (r, c) = do
  pc <- runParser commitP "git" . cs . commitMessage $ c

  pure (r, c, pc)

generateReport :: [ParsedGroup] -> ChangeReport
generateReport = foldl inner def
  where
    inner acc (r, c, x) = case x of
      PCMisc miscCommit -> crMisc <>~ [(r, c, miscCommit)] $ acc
      PCMerge mergeCommit -> crMerges <>~ [(r, c, mergeCommit)] $ acc
      PCConventional commit -> (case commit ^. ccType of
        CTFeature -> crFeatures
        CTFix -> crFixes
        CTDocs -> crDocs
        CTStyle -> crStyles
        CTRefactor -> crRefactors
        CTChore -> crChores
        CTTest -> crTests) <>~ [(r, c, commit)] $ acc
