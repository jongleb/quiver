#!/bin/sh
# Installs the quiver release binary:
#   curl -fsSL https://raw.githubusercontent.com/jongleb/quiver/main/install.sh | sh
# QUIVER_VERSION picks a release (default: the latest), QUIVER_INSTALL_DIR the
# directory (default: ~/.local/bin).
set -eu

repository=https://github.com/jongleb/quiver
directory=${QUIVER_INSTALL_DIR:-$HOME/.local/bin}

fail() {
  echo "quiver install: $*" >&2
  exit 1
}

case "$(uname -s)/$(uname -m)" in
  Linux/x86_64 | Linux/amd64) target=x86_64-unknown-linux-gnu ;;
  Linux/aarch64 | Linux/arm64) target=aarch64-unknown-linux-gnu ;;
  Darwin/arm64) target=aarch64-apple-darwin ;;
  Darwin/x86_64)
    if [ "$(sysctl -in sysctl.proc_translated 2>/dev/null)" = 1 ]; then
      target=aarch64-apple-darwin
    else
      fail "Intel Macs are not supported; quiver builds for Apple silicon"
    fi
    ;;
  *) fail "no release for $(uname -s) $(uname -m)" ;;
esac

if [ -n "${QUIVER_VERSION:-}" ]; then
  version=${QUIVER_VERSION#v}
else
  latest=$(curl -fsSLo /dev/null -w '%{url_effective}' "$repository/releases/latest")
  version=${latest##*/v}
  [ "$version" != "$latest" ] || fail "cannot find the latest release"
fi

archive=quiver-$version-$target.tar.gz
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

echo "Downloading quiver $version for $target"
curl -fsSLo "$work/$archive" "$repository/releases/download/v$version/$archive"
curl -fsSLo "$work/SHA256SUMS" "$repository/releases/download/v$version/SHA256SUMS"

expected=$(awk -v file="$archive" '$2 == file { print $1 }' "$work/SHA256SUMS")
[ -n "$expected" ] || fail "SHA256SUMS has no line for $archive"
if command -v sha256sum >/dev/null; then
  actual=$(sha256sum "$work/$archive" | cut -d ' ' -f 1)
else
  actual=$(shasum -a 256 "$work/$archive" | cut -d ' ' -f 1)
fi
[ "$actual" = "$expected" ] || fail "checksum mismatch for $archive"

tar -xzf "$work/$archive" -C "$work"
mkdir -p "$directory"
cp "$work/quiver-$version-$target/quiver" "$directory/quiver.new"
chmod 755 "$directory/quiver.new"
mv "$directory/quiver.new" "$directory/quiver"
echo "Installed $("$directory/quiver" --version | sed 's/^/quiver /') to $directory/quiver"

case ":$PATH:" in
  *":$directory:"*) ;;
  *) echo "Add $directory to PATH to run it as quiver" ;;
esac
