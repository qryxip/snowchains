#!/bin/bash

OPENSSL_VERSION=1.0.2u

export CC=/usr/bin/musl-gcc
export C_INCLUDE_PATH=/usr/local/musl/include

sudo mkdir -p "$C_INCLUDE_PATH"
sudo ln -s /usr/include/linux "$C_INCLUDE_PATH/"
sudo ln -s /usr/include/x86_64-linux-gnu/asm "$C_INCLUDE_PATH/"
sudo ln -s /usr/include/asm-generic "$C_INCLUDE_PATH/"

curl -sS "https://www.openssl.org/source/openssl-$OPENSSL_VERSION.tar.gz" --retry 10 | tar -xzC /tmp

cd "/tmp/openssl-$OPENSSL_VERSION"
./Configure --prefix=/usr/local/openssl linux-x86_64
make
sudo make install

echo '::set-env name=PKG_CONFIG_ALLOW_CROSS::1'
echo '::set-env name=OPENSSL_DIR::/usr/local/openssl'
echo '::set-env name=OPENSSL_STATIC::1'
