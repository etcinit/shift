{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Shift.Parsers where

import           Control.Monad      (void)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE

import Data.HashSet            (HashSet, fromList)
import Data.String.Conversions (cs)
import Text.Megaparsec
import Text.Megaparsec.Text

import Shift.Types

commit :: Parser ParsedCommit
commit
  = (PCConventional <$> conventionalCommit)
  <|> try (PCMerge <$> mergeCommit)
  <|> (PCMisc <$> miscCommit)

spaced :: Parser a -> Parser a
spaced = (*>) $ many (some spaceChar <|> eol)

oneParserOf :: NE.NonEmpty (Parser a) -> Parser a
oneParserOf (NE.reverse -> (x :| xs)) = foldl (\acc x -> try x <|> acc) x xs

manyCharsTill :: Parser end -> Parser [Char]
manyCharsTill = manyTill anyChar

someCharsTill :: Parser end -> Parser [Char]
someCharsTill = someTill anyChar

conventionalCommit :: Parser ConventionalCommit
conventionalCommit = do
  cType <- commitType
  void (char '(')
  cScope <- someCharsTill (try $ string "): ")
  cSubject <- someCharsTill (choice [eol >> pure (), eof])

  cBody <- spaced . manyCharsTill . lookAhead $ do
    skipMany (try breakingChange)
    skipMany (try ticketChange)
    many eol
    eof

  cBreakingChanges <- many (try breakingChange)
  cTicketChanges <- many (try ticketChange)

  pure $ ConventionalCommit
    cType
    (cs cScope)
    (cs cSubject)
    (cs cBody)
    cBreakingChanges
    cTicketChanges

commitType :: Parser CommitType
commitType = do
  cType <- oneParserOf . fmap string $
    "feat" :|
      [ "fix", "bug", "docs", "doc", "style", "refactor", "ref", "test"
      , "chore"
      ]

  pure $ case cType of
    "feat" -> CTFeature
    "fix" -> CTFix
    "bug" -> CTFix
    "docs" -> CTDocs
    "doc" -> CTDocs
    "style" -> CTStyle
    "refactor" -> CTRefactor
    "ref" -> CTRefactor
    "test" -> CTTest
    "chore" -> CTChore
    _ -> CTFeature

breakingChange :: Parser BreakingChange
breakingChange = do
  void . spaced . string $ "BREAKING CHANGE: "

  BreakingChange
    <$> (cs <$> manyCharsTill eol)
    <*> (cs <$> manyCharsTill (spaced eof <|> (skipSome ticketChange *> eof)))

ticketChange :: Parser (HashSet TicketChange)
ticketChange = do
  tcAction <- spaced $ manyCharsTill (string ": ")
  tcTickets <- some (char '#' *> manyCharsTill (some spaceChar <|> eol))

  pure . fromList $ (\x -> (cs tcAction, cs x)) <$> tcTickets

mergeCommit :: Parser MergeCommit
mergeCommit = MergeCommit . cs
  <$> (string "Merge " *> manyCharsTill (skipSome eol <|> eof))

miscCommit :: Parser MiscCommit
miscCommit = MiscCommit . cs <$> manyCharsTill (skipSome eol <|> eof)
