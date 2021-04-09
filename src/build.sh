#!/bin/bash

# cd to the directory containing this script
cd "$(dirname "$0")"

MAIN="wattsi"
SRC=""
#MODE="DEBUG"
#MODE="FAST-DEBUG"
#MODE="FAST"
#MODE="VALGRIND-DEBUG"
#MODE="PROFILE"
#MODE="MEMCHECK"
MODE="RELEASE"

PATHS="-Fu${SRC}html -Fi${SRC}html -Fi${SRC}html/entities.inc"
DEFINES="-dUSEROPES -dLINES -dPARSEERROR"

mkdir -p ../bin

VERSION_FILE="version.inc"
if [[ -f "$VERSION_FILE" ]]; then
  echo "$VERSION_FILE exists: version $(cat $VERSION_FILE)"
else
  echo "$VERSION_FILE must exist"
  exit 1
fi

if which xcrun > /dev/null 2>&1; then
  DEFINES="${DEFINES} -XR$(xcrun --show-sdk-path)"
fi

. ${SRC}lib/compile.sh
