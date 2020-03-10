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

echo "Writing $VERSION_FILE"
# If you update the fallback below also update WATTSI_LATEST in
# https://github.com/whatwg/html-build/blob/master/build.sh
(git rev-list --count HEAD || echo "88") > "$VERSION_FILE"
. ${SRC}lib/compile.sh
echo "Removing $VERSION_FILE"
rm "$VERSION_FILE"
