ARG CROSS_BASE
FROM rustembedded/cross:$CROSS_BASE
RUN apt-get update \
 && apt-get install -y capnproto \
 && rm -rf /var/lib/apt/lists/*
