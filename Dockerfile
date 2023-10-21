FROM haskell:latest as backend

WORKDIR /app

COPY . /app

RUN apt-get clean && apt-get update && apt-get --allow-unauthenticated install -y libtinfo-dev

RUN cabal update
RUN cabal build

FROM alpine:latest

RUN apk add libc6-compat gmp

ENV GHC 9.8.1

COPY --from=backend /app/dist-newstyle/build/x86_64-linux/ghc-${GHC}/todo-scaner-0.1.0.0/x/todo-scaner/build/todo-scaner/todo-scaner /srv/todo-scaner

WORKDIR /srv

ENTRYPOINT ["/srv/todo-scaner"]