{-# LANGUAGE OverloadedStrings #-}

module Shift.Parsers where

import Text.Megaparsec
import Text.Megaparsec.Text
import Data.HashSet (HashSet, fromList)
import Data.String.Conversions (cs)
import Control.Monad (void)

import Shift.Types

commitP :: Parser ParsedCommit
commitP
  = (PCConventional <$> conventionalCommitP)
  <|> try (PCMerge <$> mergeCommitP)
  <|> (PCMisc <$> miscCommitP)

spaced :: Parser a -> Parser a
spaced = (*>) $ many (some spaceChar <|> eol)

conventionalCommitP :: Parser ConventionalCommit
conventionalCommitP = do
  cType <- commitTypeP
  void (char '(')
  cScope <- someTill anyChar (try $ string "): ")
  cSubject <- someTill anyChar (choice [eol >> pure (), eof])

  cBody <- spaced . manyTill anyChar . lookAhead $
    ( skipMany (try breakingChangeP)
      *> skipMany (try ticketChangeP)
      *> many eol
      *> eof
    )

  cBreakingChanges <- many (try breakingChangeP)
  cTicketChanges <- many (try ticketChangeP)

  pure $ ConventionalCommit
    cType
    (cs cScope)
    (cs cSubject)
    (cs cBody)
    cBreakingChanges
    cTicketChanges

commitTypeP :: Parser CommitType
commitTypeP = do
  cType <- try (string "feat")
    <|> try (string "fix")
    <|> try (string "bug") -- Common typo
    <|> try (string "docs")
    <|> try (string "doc") -- Common typo
    <|> try (string "style")
    <|> try (string "refactor")
    <|> try (string "ref") -- Common typo
    <|> try (string "test")
    <|> string "chore"

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

breakingChangeP :: Parser BreakingChange
breakingChangeP = do
  void . spaced . string $ "BREAKING CHANGE: "

  BreakingChange
    <$> (cs <$> manyTill anyChar eol)
    <*> (cs <$> manyTill anyChar 
      (spaced eof <|> (skipSome ticketChangeP *> eof))
    )

ticketChangeP :: Parser (HashSet TicketChange)
ticketChangeP = do
  tcAction <- spaced $ manyTill anyChar (string ": ")
  tcTickets <- some (char '#' *> manyTill anyChar (some spaceChar <|> eol))

  pure . fromList $ (\x -> (cs tcAction, cs x)) <$> tcTickets

mergeCommitP :: Parser MergeCommit
mergeCommitP = MergeCommit . cs <$> (string "Merge " *> manyTill anyChar (skipSome eol <|> eof))

miscCommitP :: Parser MiscCommit
miscCommitP = MiscCommit . cs <$> manyTill anyChar (skipSome eol <|> eof)
