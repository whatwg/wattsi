FROM debian:stable-slim

RUN apt-get update && \
    apt-get install -y --no-install-recommends fp-compiler fp-units-fcl fp-units-net libc6-dev

COPY src /whatwg/wattsi/src
RUN mkdir -p /whatwg/wattsi/bin

WORKDIR /whatwg/wattsi/src
RUN ./build.sh

RUN rm -rf /whatwg/wattsi/src
RUN apt-get purge -y fp-compiler fp-units-fcl fp-units-net libc6-dev && \
    apt-get autoremove -y

ENTRYPOINT ["/whatwg/wattsi/bin/wattsi"]
