FROM fpco/pid1:22.04

RUN apt update -y && apt install -y curl

RUN curl -sL https://github.com/casey/just/releases/download/1.14.0/just-1.14.0-x86_64-unknown-linux-musl.tar.gz | tar xz && mv just /usr/local/bin/just

RUN mkdir /home/sibi

WORKDIR /home/sibi

COPY justfile /home/sibi/justfile
