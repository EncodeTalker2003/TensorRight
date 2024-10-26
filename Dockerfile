FROM ubuntu:22.04

ARG TARGETARCH

ARG CVC5_ARCH_IDENTIFIER_arm64="arm64"
ARG CVC5_ARCH_IDENTIFIER_amd64="x86_64"
ARG CONDA_ARCH_IDENTIFIER_arm64="aarch64"
ARG CONDA_ARCH_IDENTIFIER_amd64="x86_64"

RUN apt-get update \
  && apt-get install -y \
  curl wget unzip git z3 \
  build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*


RUN export CVC5_ARCH_IDENTIFIER0="CVC5_ARCH_IDENTIFIER_$TARGETARCH" && \
    eval CVC5_ARCH_IDENTIFIER="\$$CVC5_ARCH_IDENTIFIER0" && \
    wget -O ~/cvc5-Linux-static.zip https://github.com/cvc5/cvc5/releases/download/cvc5-1.2.0/cvc5-Linux-${CVC5_ARCH_IDENTIFIER}-static.zip && \
    unzip ~/cvc5-Linux-static.zip && \
    rm ~/cvc5-Linux-static.zip && \
    mv cvc5-Linux-*-static ~/cvc5 && \
    ln -s ~/cvc5/bin/cvc5 /usr/bin/cvc5

# Install Python
RUN export CONDA_ARCH_IDENTIFIER0="CONDA_ARCH_IDENTIFIER_$TARGETARCH" && \
    eval CONDA_ARCH_IDENTIFIER="\$$CONDA_ARCH_IDENTIFIER0" && \
    curl -sL -o ~/miniconda.sh https://repo.anaconda.com/miniconda/Miniconda3-py37_4.12.0-Linux-${CONDA_ARCH_IDENTIFIER}.sh && \
    bash ~/miniconda.sh -b -p ~/.conda && \
    rm ~/miniconda.sh && \
    ~/.conda/bin/conda init

RUN ~/.conda/bin/conda run -n base \
  conda install -y python=3.10 numpy=1.26.4 matplotlib=3.8.4
RUN ~/.conda/bin/conda run -n base \
  conda clean -y --all

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH="/root/.ghcup/bin:${PATH}"
ENV LANG=C.UTF-8
RUN ghcup install stack

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
