FROM debian:stable-slim

RUN apt-get update && \
    apt-get install -y --no-install-recommends gnucobol socat && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /work
COPY main.cob /work/
RUN cobc -x main.cob -o main

CMD ["sh", "-c", \
     "rm -f /var/run/cobol/cobol.sock && \
      socat UNIX-LISTEN:/var/run/cobol/cobol.sock,fork EXEC:/work/main"]
