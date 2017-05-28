FROM debian:jessie

RUN apt-get update && apt-get install -y \
    curl \
    sudo \
    bzip2 \
    make \
    libffi6 \
    wget \
    pkg-config \
    build-essential \
    libffi-dev \
    python \
 && rm -rf /var/lib/apt/lists/*

ENV LISP=sbcl

RUN curl https://raw.githubusercontent.com/luismbo/cl-travis/master/install.sh | sh

RUN rm -f /root/.config/common-lisp/source-registry.conf.d/cl-travis.conf
