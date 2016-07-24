module Shift.Processing where

import Control.Lens            ((<>~), (^.))
import Data.Default            (def)
import Data.Git                (Commit, Ref, commitMessage)
import Data.String.Conversions (cs)
import Text.Megaparsec         (ParseError, runParser)

import qualified Shift.Parsers as P
import           Shift.Types

parseCommit :: (Ref, Commit) -> Either ParseError ParsedGroup
parseCommit (r, c) = do
  pc <- runParser P.commit "git" . cs . commitMessage $ c

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
