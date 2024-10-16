FROM ubuntu:22.04

RUN apt-get update \
  && apt-get install -y \
  curl wget unzip git z3 \
  build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH="/root/.ghcup/bin:${PATH}"
ENV LANG=C.UTF-8

RUN wget -O ~/cvc5-Linux-x86_64-static.zip https://github.com/cvc5/cvc5/releases/download/cvc5-1.2.0/cvc5-Linux-x86_64-static.zip
RUN unzip ~/cvc5-Linux-x86_64-static.zip && rm ~/cvc5-Linux-x86_64-static.zip && mv cvc5-Linux-x86_64-static ~/cvc5
RUN ln -s ~/cvc5/bin/cvc5 /usr/bin/cvc5

# Install Python
RUN curl -sL -o ~/miniconda.sh https://repo.anaconda.com/miniconda/Miniconda3-py37_4.12.0-Linux-x86_64.sh && \
  bash ~/miniconda.sh -b -p ~/.conda && \
  rm ~/miniconda.sh && \
  ~/.conda/bin/conda init

RUN ~/.conda/bin/conda run -n base \
  conda install -y python=3.10 numpy=1.26.4 matplotlib=3.8.4
RUN ~/.conda/bin/conda run -n base \
  conda clean -y --all

WORKDIR /home
RUN mkdir -p /home/tr/

COPY rules/ /home/tr/rules
COPY src/ /home/tr/src
COPY test/ /home/tr/test
COPY package.yaml /home/tr/package.yaml
COPY stack.yaml /home/tr/stack.yaml
COPY Makefile /home/tr/Makefile
COPY runall.sh /home/tr/runall.sh

WORKDIR /home/tr
RUN stack build
