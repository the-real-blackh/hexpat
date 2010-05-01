#!/bin/sh

SUITE=./dist/build/testsuite/testsuite

set -e
rm -f testsuite.tix

if [ ! -f $SUITE ]; then
    cat <<EOF
Testsuite executable not found, please run:
    cabal configure -ftest
then
    cabal build
EOF
    exit;
fi

./dist/build/testsuite/testsuite -j4

DIR=dist/hpc

rm -Rf $DIR
mkdir -p $DIR

EXCLUDES="Main \
          Text.XML.Expat.Tests \
          Text.XML.Expat.UnitTests \
          Text.XML.Expat.Proc.Tests \
          Text.XML.Expat.Cursor.Tests"
EXCL=""

for m in $EXCLUDES; do
    EXCL="$EXCL --exclude=$m"
done

hpc markup $EXCL --destdir=$DIR testsuite >/dev/null 2>&1

rm -f testsuite.tix

cat <<EOF

Test coverage report written to $DIR.
EOF
