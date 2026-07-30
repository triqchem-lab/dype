{-# OPTIONS_GHC -Wunused-imports #-}

-- | Emacs mode setup (stub for dual-package compilation).
-- Exports all functions needed by agda-mode/Main.hs, no external deps.

module Agda.Setup.EmacsMode
  ( help
  , locateFlag
  , printEmacsModeFile
  , setupFlag
  , setupDotEmacs
  , compileFlag
  , compileElispFiles
  , inform
  )
where

import System.IO ( hPutStr, stderr )

help :: String
help = "Usage: agda-mode [OPTION...]\n\n  --locate   Print the path to the Emacs mode file.\n  --setup    Set up Emacs for Agda.\n  --compile  Compile the Emacs mode's Lisp files.\n"

locateFlag, setupFlag, compileFlag :: String
locateFlag = "--locate"
setupFlag  = "--setup"
compileFlag = "--compile"

printEmacsModeFile :: IO ()
printEmacsModeFile = putStrLn "agda2.el"

setupDotEmacs :: String -> IO ()
setupDotEmacs _ = inform "Setup complete (stub)."

compileElispFiles :: IO ()
compileElispFiles = inform "Compilation skipped (stub)."

inform :: String -> IO ()
inform = hPutStr stderr
