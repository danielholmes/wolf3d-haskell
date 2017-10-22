FROM ubuntu:16.04

RUN add-apt-repository -y ppa:hvr/ghc
RUN apt-get update
RUN apt-get install -y ghc-8.0.2 libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN mkdir -p ~/.local/bin
RUN export PATH=$HOME/.local/bin:$PATH
RUN curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
RUN stack config set system-ghc --global true
RUN export PATH=/opt/ghc/8.0.2/bin:$PATH