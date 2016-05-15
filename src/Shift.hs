module Shift (shiftMain, module X) where

import Shift.CLI as X
import Shift.Git as X
import Shift.Rendering as X
import Shift.Types as X

import Control.Lens ((^.))
import Options.Applicative

-- | The main CLI entrypoint.
shiftMain :: IO ()
shiftMain = do
  currentOptions <- execParser opts

  case currentOptions ^. soCommand of
    GenerateCommand -> tempMain currentOptions

  where
    opts = info (helper <*> shiftOptions)
      ( fullDesc
      <> progDesc "Execute the given COMMAND"
      <> header "shift - A change log generator"
      )

