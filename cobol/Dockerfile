FROM debian:stable-slim

# ── Open-COBOL-ESQL をビルド ────────────────────────────
COPY setup.sh /tmp/
RUN chmod +x /tmp/setup.sh && /tmp/setup.sh && rm /tmp/setup.sh

# ODBC ドライバ
COPY odbcinst.ini /etc/odbcinst.ini

# ESQL ランタイムソースの場所
ENV OCESQL_SRC=/usr/local/share/ocesql/source

# アプリ配置
WORKDIR /work
COPY main.cob modules/ /work/

# ビルドスクリプトをコピーして実行
COPY build.sh /work/
RUN chmod +x /work/build.sh && /work/build.sh

CMD ["sh", "-c", \
     "rm -f /var/run/cobol/cobol.sock && \
      socat UNIX-LISTEN:/var/run/cobol/cobol.sock,fork EXEC:/work/main"]
