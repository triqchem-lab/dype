#!/bin/bash

# Change dype version in all files.

# Usage: From the repository root directory run
#   ./scripts/change-version.bash old_version new_version

old_version="$1"
new_version="$2"

files+='dype.cabal '
files+='mk/versions.mk '
files+='src/dype-core/dype-core.cabal '
files+='src/dype-core/setup/Agda/Version.hs '
files+='src/dype-core/data/emacs-mode/agda2-mode.el '
files+='src/dype-core/data/emacs-mode/agda2-mode-pkg.el '
files+='src/dype-core/data/latex/agda.sty '
files+='src/dype-core/size-solver/size-solver.cabal '
files+='src/dype-core/agda-bisect/dype-bisect.cabal '

if [ "$2" == "" -o "$1" == "-h" -o "$1" == "--help" ]; then
  echo "Usage: $0 OLD NEW"
  echo "Replaces version string OLD by NEW in the following files:"
  for i in $files; do
    echo "- $i"
  done
  echo "Example: $0 2.9.0 2.9.1"
  exit 1
fi

for i in $files; do
    sed -i "s/$old_version/$new_version/g" $i
done
