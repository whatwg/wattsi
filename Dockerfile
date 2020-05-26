FROM debian:stable-slim AS builder

RUN apt-get update && \
    apt-get install -y --no-install-recommends fp-compiler fp-units-fcl fp-units-net libc6-dev

COPY src /whatwg/wattsi/src
RUN /whatwg/wattsi/src/build.sh

FROM gcr.io/distroless/base
COPY --from=builder /whatwg/wattsi/bin /whatwg/wattsi/bin

ENTRYPOINT ["/whatwg/wattsi/bin/wattsi"]
