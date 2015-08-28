# Wattsi

Wattsi is a build tool for creating the [HTML Standard](http://html.spec.whatwg.org/multipage/) from its [source](https://github.com/whatwg/html). It works in concert with [other scripts](https://github.com/whatwg/html-build).

## Building

Wattsi is written in [Free Pascal](http://www.freepascal.org/), and uses features from the latest release candidate versions. You'll need to install the 3.0.0-rc1 FreePascal compiler from ftp://freepascal.stack.nl/pub/fpc/beta/3.0.0-rc1/ to build it. With that installed, you should be able to run `./build.sh` to create the wattsi executable and supporting shared libraries.

We hope to in the future provide precompiled wattsi binaries, built via continuous integration, for Wattsi. Or even a web service that allows you to run Wattsi by POSTing to a given URL. If you think you can help out with these tasks, file an issue to get the discussion started!
