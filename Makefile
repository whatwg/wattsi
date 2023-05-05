SHELL=/bin/bash -o pipefail
.PHONY: clean docker manual
.DEFAULT_GOAL := manual

clean:
	rm -rf src/version.inc
	rm -rf bin/

docker:
	git rev-list --count HEAD > src/version.inc
	docker build --tag whatwg/wattsi .

manual:
	git rev-list --count HEAD > src/version.inc
	bash ./src/build.sh
