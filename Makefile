SHELL=/bin/bash -o pipefail
.PHONY: clean docker manual

clean:
	rm -rf src/version.inc
	rm -rf bin/

docker:
	git rev-list --count HEAD > src/version.inc
	docker pull whatwg/wattsi || true
	docker build --pull --cache-from whatwg/wattsi --tag whatwg/wattsi .

manual:
	bash ./src/build.sh
