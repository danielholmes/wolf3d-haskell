FROM ubuntu:16.04

RUN apt-get update
RUN apt-get install -y software-properties-common
RUN apt-get update
RUN apt-get install -y curl libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN mkdir -p ~/.local/bin
RUN export PATH=$HOME/.local/bin:$PATH
RUN curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'

RUN git clone https://github.com/danielholmes/wolf3d-haskell.git /root/wolf3d
RUN cd /root/wolf3d
RUN stack --no-terminal --skip-ghc-check setup
RUN stack --no-terminal --skip-ghc-check test --pedantic
