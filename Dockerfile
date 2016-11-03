# docker build -t tapl .
# docker run -it tapl zsh

FROM fedora:24

RUN dnf install -y ocaml wget vim zsh git mercurial ghc cabal-install tar zlib zlib-devel ghc-parsec make findutils man glibc-langpack-en

RUN mkdir -p /root/tapl-original
WORKDIR /root/tapl-original
RUN wget http://www.cis.upenn.edu/~bcpierce/tapl/checkers/arith.tar.gz
RUN wget http://www.cis.upenn.edu/~bcpierce/tapl/checkers/untyped.tar.gz
RUN wget http://www.cis.upenn.edu/~bcpierce/tapl/checkers/simplebool.tar.gz
RUN find . -name "*.tar.gz" | xargs -L1 tar xzf
RUN rm -rf *.tar.gz

WORKDIR /root/
RUN hg clone http://firefly.cr.ie.u-ryukyu.ac.jp/hg/Members/atton/tapl
