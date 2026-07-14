# dype - Da-Yan Proof Engine
# Build stage
FROM haskell:9.14.1 AS builder

WORKDIR /build
COPY . .
RUN cabal update && cabal build all

# Runtime stage (TODO: reduce image size with multi-stage)
FROM haskell:9.14.1
WORKDIR /app
COPY --from=builder /build/dist-newstyle/build/x86_64-linux/ghc-9.14.1/dype-*/build/dype-test/dype-test /app/
COPY --from=builder /build/test/ /app/test/
CMD ["dype-test"]
