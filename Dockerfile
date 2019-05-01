FROM debian:stretch

RUN apt-get update && apt-get install -y --no-install-recommends curl ca-certificates g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg netbase
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN useradd builder
# Tells docker to run commands after this line as the builder user
USER builder
