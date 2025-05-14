#!/bin/bash
set -euo pipefail

echo "[1/3] apt install ..."
apt-get update
apt-get install -y --no-install-recommends \
  gnucobol socat git build-essential \
  autoconf automake libtool pkg-config \
  unixodbc unixodbc-dev odbc-postgresql libpq-dev \
  ca-certificates m4 perl bison flex
rm -rf /var/lib/apt/lists/*

echo "[2/3] build Open-COBOL-ESQL ..."
cd /tmp
git clone --depth 1 https://github.com/opensourcecobol/Open-COBOL-ESQL.git
cd Open-COBOL-ESQL
export CPPFLAGS="-I/usr/include/postgresql"
export LDFLAGS="-L/usr/lib"
./configure >/dev/null
make -j"$(nproc)" >/dev/null
make install >/dev/null
echo 'export COBCPY=/usr/local/share/ocesql/copy' > /etc/profile.d/ocesql.sh
chmod +x /etc/profile.d/ocesql.sh
cd /
rm -rf /tmp/Open-COBOL-ESQL

echo "[3/3] done."
