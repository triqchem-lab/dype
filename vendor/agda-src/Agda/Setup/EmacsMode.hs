-- Stub: Agda.Setup.EmacsMode — emacs mode setup (not needed for parsing)
module Agda.Setup.EmacsMode (setupFlag, compileFlag, locateFlag, help) where

setupFlag, compileFlag, locateFlag :: String
setupFlag = "--setup"
compileFlag = "--compile"
locateFlag = "--locate"

help :: String
help = ""
