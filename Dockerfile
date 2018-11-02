#--- Builder stage ---
FROM alpine:3.8 as builder

ARG target=install

RUN apk add --no-cache \
        ca-certificates \
        zlib-dev \
        musl-dev \
        g++ \
        ghc \
        tar \
        curl \
        openssl-dev \
        make

# get static version of Haskell Stack and use system ghc by default
ARG STACK_ALPINE_VERSION=1.9.1
RUN curl -sSfL https://github.com/commercialhaskell/stack/releases/download/v${STACK_ALPINE_VERSION}/stack-${STACK_ALPINE_VERSION}-linux-x86_64-static.tar.gz \
    | tar --wildcards -C /usr/local/bin --strip-components=1 -xzvf - '*/stack' && chmod 755 /usr/local/bin/stack && \
    stack config set system-ghc --global true

COPY . /src/mls-server/

RUN cd /src/mls-server/ && make ${target}

#--- Minified stage ---
FROM alpine:3.8 as deps

ARG executable
COPY --from=builder /src/mls-server/dist/${executable} /usr/bin/${executable}

RUN apk add --no-cache \
        openssl \
        gmp \
        libgcc \
        libffi \
        libstdc++ \
        ca-certificates \
        dumb-init

# ARGs are not available at runtime, create symlink at build time
# more info: https://stackoverflow.com/questions/40902445/using-variable-interpolation-in-string-in-docker
RUN ln -s /usr/bin/${executable} /usr/bin/service
ENTRYPOINT ["/usr/bin/dumb-init", "--", "/usr/bin/service"]
