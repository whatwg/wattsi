FROM debian:stable-slim AS builder

RUN apt-get update && \
    apt-get install -y --no-install-recommends fp-compiler fp-units-fcl fp-units-net libc6-dev

COPY src /imhele/wattsi/src
RUN /imhele/wattsi/src/build.sh

FROM gcr.io/distroless/base
COPY --from=builder /imhele/wattsi/bin /imhele/wattsi/bin

ENTRYPOINT ["/imhele/wattsi/bin/wattsi"]
