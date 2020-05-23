FROM debian:stable-slim

COPY src /whatwg/wattsi/src
WORKDIR /whatwg/wattsi/src

RUN apt-get update && \
    apt-get install -y --no-install-recommends fp-compiler fp-units-fcl fp-units-net libc6-dev && \
    ./build.sh && \
    rm -rf /whatwg/wattsi/src && \
    mv /whatwg/wattsi/bin/wattsi /whatwg/wattsi/wattsi && \
    rm -rf /whatwg/wattsi/bin && \
    apt-get purge -y fp-compiler fp-units-fcl fp-units-net libc6-dev && \
    apt-get autoremove -y

ENTRYPOINT ["/whatwg/wattsi/wattsi"]
