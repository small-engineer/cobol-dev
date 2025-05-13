FROM debian:stable-slim

RUN apt-get update && \
    apt-get install -y gnucobol ncat && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /work
COPY main.cob /work/
RUN cobc -x main.cob -o main

CMD ["sh", "-c", "while true; do ncat -l 6000 --exec ./main; done"]

