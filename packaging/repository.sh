#!/bin/sh
# Builds the apt and dnf/yum repositories of the release packages in $1 into
# $2, signed with the GnuPG key $3 (already in the keyring, no passphrase).
# The site is served at https://jongleb.github.io/quiver/.
set -eu

packages=$1
site=$2
key=$3
base=https://jongleb.github.io/quiver

rm -rf "$site"
mkdir -p "$site/deb/pool/main"
cp "$packages"/*.deb "$site/deb/pool/main/"
(
  cd "$site/deb"
  for arch in amd64 arm64; do
    mkdir -p "dists/stable/main/binary-$arch"
    apt-ftparchive --arch "$arch" packages pool >"dists/stable/main/binary-$arch/Packages"
    gzip -9kf "dists/stable/main/binary-$arch/Packages"
  done
  apt-ftparchive \
    -o APT::FTPArchive::Release::Origin=quiver \
    -o APT::FTPArchive::Release::Label=quiver \
    -o APT::FTPArchive::Release::Suite=stable \
    -o APT::FTPArchive::Release::Codename=stable \
    -o "APT::FTPArchive::Release::Architectures=amd64 arm64" \
    -o APT::FTPArchive::Release::Components=main \
    release dists/stable >dists/stable/Release
  gpg --batch --yes --local-user "$key" --clearsign \
    -o dists/stable/InRelease dists/stable/Release
  gpg --batch --yes --local-user "$key" --armor --detach-sign \
    -o dists/stable/Release.gpg dists/stable/Release
)

for arch in x86_64 aarch64; do
  mkdir -p "$site/rpm/$arch"
  cp "$packages"/*."$arch".rpm "$site/rpm/$arch/"
  rpmsign --addsign --define "_gpg_name $key" "$site/rpm/$arch"/*.rpm
  createrepo_c "$site/rpm/$arch"
  gpg --batch --yes --local-user "$key" --armor --detach-sign \
    -o "$site/rpm/$arch/repodata/repomd.xml.asc" "$site/rpm/$arch/repodata/repomd.xml"
done

gpg --export "$key" >"$site/quiver.gpg"
gpg --armor --export "$key" >"$site/quiver.asc"
cat >"$site/quiver.repo" <<EOF
[quiver]
name=quiver
baseurl=$base/rpm/\$basearch
enabled=1
gpgcheck=1
repo_gpgcheck=1
gpgkey=$base/quiver.asc
EOF
touch "$site/.nojekyll"
