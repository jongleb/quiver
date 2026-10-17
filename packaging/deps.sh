#!/bin/sh
# Builds the static OpenSSL and zstd a release binary links, into $1.
set -eu

prefix=$1
openssl=3.5.9
openssl_sha256=603f5602e2eef00d77fbd429d34dcd5822bb301757a1bc9cdb24c670f1eb859a
zstd=1.5.7
zstd_sha256=eb33e51f49a15e023950cd7825ca74a4a2b43db8354825ac24fc1b7ee09e6fa3

if [ -f "$prefix/.built-$openssl-$zstd" ]; then exit 0; fi

jobs=$(getconf _NPROCESSORS_ONLN)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

fetch() {
  curl -fsSLo "$work/$1" "$2"
  if command -v sha256sum >/dev/null; then
    echo "$3  $work/$1" | sha256sum -c -
  else
    echo "$3  $work/$1" | shasum -a 256 -c -
  fi
  tar -xzf "$work/$1" -C "$work"
}

fetch openssl.tar.gz \
  "https://github.com/openssl/openssl/releases/download/openssl-$openssl/openssl-$openssl.tar.gz" \
  "$openssl_sha256"
# The system's own configuration and roots stay where most systems keep them;
# the stubs also look for the usual bundles (lib/openssl/openssl_stubs.c).
(cd "$work/openssl-$openssl" &&
  ./Configure no-shared no-tests no-docs --prefix="$prefix" --libdir=lib \
    --openssldir=/etc/ssl &&
  make -j"$jobs" build_libs &&
  make install_dev)
mkdir -p "$prefix/licenses"
cp "$work/openssl-$openssl/LICENSE.txt" "$prefix/licenses/openssl.txt"

fetch zstd.tar.gz \
  "https://github.com/facebook/zstd/releases/download/v$zstd/zstd-$zstd.tar.gz" \
  "$zstd_sha256"
make -C "$work/zstd-$zstd/lib" -j"$jobs" CFLAGS="-O3 -fPIC" PREFIX="$prefix" \
  install-static install-pc install-includes
cp "$work/zstd-$zstd/LICENSE" "$prefix/licenses/zstd.txt"

touch "$prefix/.built-$openssl-$zstd"
