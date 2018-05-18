# Wattsi

Wattsi is a build tool for creating the [HTML Standard](https://html.spec.whatwg.org/multipage/) from its [source](https://github.com/whatwg/html). It works in concert with [other scripts](https://github.com/whatwg/html-build).

## Features

Currently:
 * Number the sections
 * Create Table of Contents
 * Cross-reference `<span>` and `<code>` to `<dfn>`
 * Create small TOC
 * Cross-reference back (`<dfn>` menu)
 * Strip out unused references
 * Spec splitting

## Building

Wattsi is written in [Free Pascal](https://www.freepascal.org/). You'll need to install the [3.0.0 FreePascal compiler](https://www.freepascal.org/download.var) or newer to build it. With that installed, you should be able to run `./build.sh` to create the `wattsi` executable and supporting shared libraries.

We hope to in the future provide precompiled wattsi binaries, built via continuous integration, for Wattsi. If you think you can help with this, please file an issue to get the discussion started! In the meantime, we do provide the [wattsi-server](https://github.com/domenic/wattsi-server) service, which allows you to upload files to a server that will run Wattsi for you.

## Building a Wattsi binary with 64-bit code

On Mac OS X (and perhaps on other systems as well), the FreePascal compiler defaults to generating 32-bit code. If for some reason you need to build a `wattsi` binary with 64-bit code, you can do so by adding `-Px86_64` to the `DEFINES` line in the [`src/build.sh`](https://github.com/whatwg/wattsi/blob/master/src/build.sh) file, so that it looks like this:

    DEFINES="-dUSEROPES -dLINES -dPARSEERROR -Px86_64"
