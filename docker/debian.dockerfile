FROM debian:bullseye-slim AS build
RUN apt-get update && apt-get -y --no-install-recommends install \
      ca-certificates ghc git haskell-stack zlib1g-dev \
 && rm -rf /var/lib/apt/lists/*
RUN git clone https://github.com/lassik/pandoc-tar.git /build/pandoc-tar
WORKDIR /build/pandoc-tar
RUN stack install --system-ghc

FROM debian:bullseye-slim
COPY --from=build /root/.local/bin/pandoc-tar /usr/local/bin/pandoc-tar
ENTRYPOINT ["/usr/local/bin/pandoc-tar"]
