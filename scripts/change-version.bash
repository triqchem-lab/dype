#!/bin/bash
# dype version bumper
set -e
VERSION=${1:?Usage: $0 <new-version>}
sed -i "s/^version:.*/version:             $VERSION/" dype.cabal
sed -i "s/^VERSION=.*/VERSION=$VERSION/" mk/versions.mk
sed -i "s/version = \".*\"/version = \"$VERSION\"/" src/dype-core/setup/Agda/Version.hs
echo "Version bumped to $VERSION"
