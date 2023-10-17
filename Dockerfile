FROM haskell:buster as backend

WORKDIR /app

COPY . /app

RUN apt-get clean && apt-get update && apt-get --allow-unauthenticated install -y libtinfo-dev

RUN cabal update
RUN cabal build

FROM alpine:latest

RUN apk add libc6-compat gmp

COPY --from=backend /app/dist-newstyle/build/x86_64-linux/ghc-9.6.3/todo-scaner-0.1.0.0/x/todo-scaner/build/todo-scaner/todo-scaner /srv/todo-scaner

WORKDIR /srv

ENTRYPOINT ["/srv/todo-scaner"]