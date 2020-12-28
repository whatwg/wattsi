# Wattsi

Wattsi is a build tool for creating the [HTML Standard](https://html.spec.whatwg.org/multipage/) from its [source](https://github.com/imhele/html). It works in concert with [other scripts](https://github.com/imhele/html-build).

## Features

Currently:
 * Number the sections
 * Create Table of Contents
 * Cross-reference `<span>` and `<code>` to `<dfn>`
 * Create small TOC
 * Cross-reference back (`<dfn>` menu)
 * Strip out unused references
 * Check for missing references
 * Spec splitting
 * Add output for `<wpt>` elements
 * Add MDN annotations
 * Add syntax-highlighting markup to `<pre>` contents

## Wattsi syntax

For documentation on the "Wattsi language", e.g. things like `data-x` or `w-nodev`, see [Syntax.md](./Syntax.md).

## Building and running Wattsi with Docker

The easiest way to use Wattsi is via [Docker](https://www.docker.com/). Once you have Docker, you can download and run a copy of Wattsi from [Docker Hub](https://hub.docker.com/r/imhele/wattsi) using

```bash
docker run imhele/wattsi
```

The [HTML build tools](https://github.com/imhele/html-build) will automatically attempt to use this form if they cannot find a locally-installed copy of Wattsi.

If you're developing Wattsi, you can build and test it in Docker with the command `make docker` and `docker run imhele/wattsi`. The latter accepts any Wattsi arguments, e.g. `docker run imhele/wattsi --version`.

## Building and running Wattsi manually

With the [Free Pascal Compiler (fpc)](https://www.freepascal.org/) installed, you should be able to run `make manual` to create the `wattsi` executable and supporting shared libraries.

The `wattsi` executable is at `./bin/wattsi`. Tools such as `html-build` use `$PATH` to look for a local `wattsi` executable. In your terminal, run `export PATH=$PATH:$(pwd)/bin`. In the same terminal tab, you can now run tools that use the built `wattsi` executable.

For guidance on installing fpc, see the next section.

### Installing the Free Pascal Compiler (fpc)

Wattsi is written in [Free Pascal](https://www.freepascal.org/), so to build Wattsi, you'll need version 3.0.4 or later of the Free Pascal Compiler (fpc). You can get fpc by [downloading a freepascal.org upstream release](https://www.freepascal.org/download.var) — but it’s recommended that you instead install fpc using a package manager.

#### Installing fpc on Debian and Ubuntu

On Debian and Ubuntu and any other Debian-derived systems, run `apt install` to install the necessary packages:

```bash
apt install fp-compiler fp-units-fcl fp-units-net libc6-dev
```

#### Installing fpc on macOS using Homebrew

On macOS, install [Homebrew](https://brew.sh/) and the homebrew fpc 3.0.4 *revision 1* (3.0.4_1) or later package. The *revision 1* there is important — the initial 3.0.4 package will not work as expected on macOS Mojave (10.14) or later.

```bash
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install fpc
```

#### Installing fpc on macOS from a freepascal.org upstream release

If you don’t want to use homebrew but instead prefer to install a freepascal.org upstream release, you must get fpc 3.0.4a or later. The *“a”* there is important — the initial 3.0.4 package will not work as expected on macOS Mojave (10.14) or later.

https://sourceforge.net/projects/freepascal/files/Mac%20OS%20X/3.0.4/fpc-3.0.4a.intel-macosx.dmg/download

#### Installing XCode and the XCode command-line tools

On macOS, you may also need to have [XCode](https://developer.apple.com/xcode/) and the latest [XCode command-line tools](https://developer.apple.com/download/more/) installed.

If you already have XCode installed, you can ensure you have the latest XCode command-line tools by running the following command:

```bash
xcode-select --install
```
