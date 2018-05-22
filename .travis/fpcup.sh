#!/bin/sh
#
# Install FreePascal compiler (fpc) via fpcup toolkit
#

# get and unpack a fpcup tarball first
TARBALL=fpcup.zip
wget https://github.com/n1tehawk/fpcup/archive/master.zip -O ${TARBALL}
unzip -q -d ${HOME} ${TARBALL}
FPCUPDIR=${HOME}/fpcup-master

# for some reason, the repo doesn't have the bin/ files executable, fix that
chmod -R +x ${FPCUPDIR}/bin/*
#ls -lR ${FPCUPDIR}/bin/*-linux

#FPCUP=${FPCUPDIR}/bin/i386-linux/fpcup_linux_x86
FPCUP=${FPCUPDIR}/bin/x86_64-linux/fpcup_linux_x64

# dry run / help output
#${FPCUP} --help
# build FPC
${FPCUP} --noconfirm --fpcdir=${FPCDIR} --only=fpc
