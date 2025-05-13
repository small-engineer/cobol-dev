FROM httpd:2.4

RUN apt-get update && \
    apt-get install -y --no-install-recommends gcc apache2-dev make && \
    rm -rf /var/lib/apt/lists/*

COPY mod_cobol_server.c /usr/local/src/mod_cobol_server.c
COPY Makefile /usr/local/src/Makefile

RUN cd /usr/local/src && make && make install

COPY httpd.conf /usr/local/apache2/conf/httpd.conf
