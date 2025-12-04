#!/bin/bash

# cd to the directory containing this script
cd "$(dirname "$0")"

#variables supporting compile.sh, other software/variables
	MAIN="wattsi"
	SRC=""
	#MODE="DEBUG"
	#MODE="FAST-DEBUG"
	#MODE="FAST"
	#MODE="VALGRIND-DEBUG"
	#MODE="PROFILE"
	#MODE="MEMCHECK"
	MODE="RELEASE"

#Incrementally append variable to comment each part 
PATHS="-Fu${SRC}html"							#-Fu to add unit search paths
PATHS=${PATHS}" -Fi${SRC}html"					#-Fi to add include file search paths
PATHS=${PATHS}" -Fi${SRC}html/entities.inc"		#"
												# not present: -Fl option to add library search paths

#Incrementally append variable to comment each part
DEFINES="-dUSEROPES"							#observed in 3 files src/html/htmlparser.pas, webdom.pas, dom.pas
DEFINES=${DEFINES}" -dLINES"					#observed in 1 file src/html/htmlparser.pas
DEFINES=${DEFINES}" -dPARSEERROR"				#observed in 2 files: src/html/htmlparser.pas, test.pas


mkdir -p ../bin

#TODO: Are there any software requirements, configurations, or releases per year that explain why the VERSION_FILE must exist? If not, could it be a warning or recommendation?
VERSION_FILE="version.inc"
if [[ -f "$VERSION_FILE" ]]; then
  echo "$VERSION_FILE exists: version $(cat $VERSION_FILE)"
else
  echo "$VERSION_FILE must exist"
  exit 1
fi

#Fix wattsi compilation on macOS Big Sur. [from hober].
if which xcrun > /dev/null 2>&1; then
  DEFINES="${DEFINES} -XR$(xcrun --show-sdk-path)"
fi

. ${SRC}lib/compile.sh
