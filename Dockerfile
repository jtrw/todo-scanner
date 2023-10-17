# Use the official Haskell image from Docker Hub
FROM haskell:buster as backend

# Set the working directory inside the container
WORKDIR /app

# Copy the Haskell source code into the container
COPY . /app

# Install the necessary system packages
RUN apt-get clean && apt-get update && apt-get --allow-unauthenticated install -y libtinfo-dev

# Install the Haskell dependencies
RUN cabal update
RUN cabal build

#FROM scratch
#FROM fpco/haskell-scratch
FROM alpine:latest

RUN apk add libc6-compat gmp

COPY --from=backend /app/dist-newstyle/build/x86_64-linux/ghc-9.6.3/todo-scaner-0.1.0.0/x/todo-scaner/build/todo-scaner/todo-scaner /srv/todo-scaner

WORKDIR /srv

RUN chmod +x todo-scaner
ENTRYPOINT []
#ENTRYPOINT ["/srv/todo-scaner"]

