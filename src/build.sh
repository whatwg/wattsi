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
git rev-list --count HEAD > "$VERSION_FILE"
. ${SRC}lib/compile.sh
echo "Removing $VERSION_FILE"
rm "$VERSION_FILE"
