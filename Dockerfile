# docker build -t tapl .
# docker run -it tapl zsh

FROM fedora:24

RUN dnf install -y ocaml wget vim zsh git mercurial ghc cabal-install tar zlib zlib-devel ghc-parsec make

RUN mkdir -p /root/tapl-original
WORKDIR /root/tapl-original
RUN wget http://www.cis.upenn.edu/~bcpierce/tapl/checkers/arith.tar.gz
RUN tar xzf arith.tar.gz

WORKDIR /root/
RUN hg clone http://firefly.cr.ie.u-ryukyu.ac.jp/hg/Members/atton/tapl
