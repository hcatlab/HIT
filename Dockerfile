FROM haskell:9.6.7 AS builder

RUN set -eux; \
    apt-get update; \
    apt-get install -y --no-install-recommends curl gnupg pkg-config; \
    . /etc/os-release; \
    curl -fsSL https://www.postgresql.org/media/keys/ACCC4CF8.asc | gpg --dearmor -o /usr/share/keyrings/pgdg.gpg; \
    echo "deb [signed-by=/usr/share/keyrings/pgdg.gpg] http://apt.postgresql.org/pub/repos/apt ${VERSION_CODENAME}-pgdg main" > /etc/apt/sources.list.d/pgdg.list; \
    apt-get update; \
    apt-get install -y --no-install-recommends libpq-dev; \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY cabal.project ./
COPY hit/hit.cabal hit/
COPY hit-cli/hit-cli.cabal hit-cli/

COPY hit/src hit/src
COPY hit/app hit/app
COPY hit/test hit/test
COPY hit-cli/src hit-cli/src

RUN cabal update && \
    cabal configure --disable-documentation --disable-tests --disable-benchmarks && \
    cabal build hit:hit-server --only-dependencies

RUN cabal build hit:hit-server

RUN cabal install hit:hit-server --install-method=copy --installdir=/app/bin

FROM debian:bookworm-slim

RUN set -eux; \
    apt-get update; \
    apt-get install -y --no-install-recommends libpq5 libgmp10; \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=builder /app/bin/hit-server /app/bin/hit-server

EXPOSE 8080

ENV DATABASE_URL="postgresql://hit:hit@postgres:5432/hit"

CMD ["/app/bin/hit-server"]
