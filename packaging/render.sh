#!/bin/sh
# Fills @VERSION@ and @SHA256:<target>@ in template $1 from version $2 and
# the release's SHA256SUMS file $3; prints the result.
set -eu

template=$1
version=$2
sums=$3

script="s/@VERSION@/$version/g"
while read -r sum file; do
  target=${file#quiver-"$version"-}
  target=${target%.tar.gz}
  script="$script;s/@SHA256:$target@/$sum/g"
done <<SUMS
$(grep '\.tar\.gz$' "$sums")
SUMS
sed "$script" "$template" | {
  rendered=$(cat)
  if echo "$rendered" | grep -q '@[A-Z0-9:_.a-z-]*@'; then
    echo "$template: a placeholder has no value" >&2
    exit 1
  fi
  echo "$rendered"
}
