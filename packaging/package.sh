#!/bin/sh
# Builds the release binary for target $1 against the static libraries in
# $2 (packaging/deps.sh) and packs dist/quiver-<version>-<target>.tar.gz.
set -eu

target=$1
deps=$2
version=$(sed -n 's/^(version \(.*\))$/\1/p' dune-project)
name=quiver-$version-$target
dir=dist/$name

PKG_CONFIG_PATH="$deps/lib/pkgconfig" QUIVER_LINK=static \
  opam exec -- dune build --profile release --build-dir _build_dist bin/main.exe

rm -rf "$dir" && mkdir -p "$dir/licenses"
cp _build_dist/default/bin/main.exe "$dir/quiver"
chmod 755 "$dir/quiver"
strip "$dir/quiver"

case $(uname -s) in
  Linux) libraries=$(ldd "$dir/quiver") ;;
  Darwin) libraries=$(otool -L "$dir/quiver" | tail -n +2) ;;
esac
echo "$libraries"
if echo "$libraries" | grep -Eq 'ssl|crypto|zstd|/opt/|/usr/local/'; then
  echo "quiver links a library that is not part of the system" >&2
  exit 1
fi
test "$("$dir/quiver" --version)" = "$version"
"$dir/quiver" dump --help=plain >/dev/null

cp LICENSE README.md "$dir/"
cp "$deps/licenses/openssl.txt" "$deps/licenses/zstd.txt" "$dir/licenses/"
cp vendor/xxhash/LICENSE "$dir/licenses/xxhash.txt"
cp vendor/zmij/LICENSE "$dir/licenses/zmij.txt"
tar -czf "dist/$name.tar.gz" -C dist "$name"
echo "dist/$name.tar.gz"
