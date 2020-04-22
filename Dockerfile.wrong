# reference: https://www.fpcomplete.com/blog/2017/12/building-haskell-apps-with-docker
#
# GHC_VERSION and LTS_SLUG are defined in https://github.com/commercialhaskell/stack/blob/master/etc/dockerfiles/stack-build/lts-13.20/Dockerfile
#
# 8.6.5 is the ghc version.

FROM fpco/stack-build:lts-13.20 as build
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build --system-ghc

FROM ubuntu:16.04
RUN mkdir -p /opt/finance-server-ingester
ARG BINARY_PATH
WORKDIR /opt/finance-server-ingester
RUN apt-get update && apt-get install -y \
        ca-certificates \
        libgmp-dev \
        netbase
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-13.20/8.6.5/bin .
ENTRYPOINT ["/opt/finance-server-ingester/ingester-exe"]
CMD ["-p", ${INGESTER_PORT}, "-a", ${ALPHAVANTAGE_KEY}]
