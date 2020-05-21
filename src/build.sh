#!/bin/bash

MAIN="wattsi"
SRC=""
#MODE="DEBUG"
#MODE="FAST-DEBUG"
#MODE="FAST"
#MODE="VALGRIND-DEBUG"
#MODE="PROFILE"
#MODE="MEMCHECK"
MODE="RELEASE"

get_abs_filename() {
  # We need to get absolute paths because different parts of this script
  # manipulate files from different directories.
  echo "$(cd "$(dirname "$1")" || exit; pwd)/$(basename "$1")"
}

VERSION_FILE="$(get_abs_filename version.inc)"
PATHS="-Fu${SRC}html -Fi${SRC}html -Fi${SRC}html/entities.inc"
DEFINES="-dUSEROPES -dLINES -dPARSEERROR"

if [[ -x "$(command -v git)" ]]; then
  echo "Writing $VERSION_FILE"
  # If you update the fallback below also update WATTSI_LATEST in
  # https://github.com/whatwg/html-build/blob/master/build.sh
  git rev-list --count HEAD > "$VERSION_FILE"
elif [[ -f "$VERSION_FILE" ]]; then
  echo "$VERSION_FILE exists: version $(cat $VERSION_FILE)"
else
  echo "$VERSION_FILE must exist, or git must be installed so we can infer it"
  exit 1
fi

. ${SRC}lib/compile.sh
