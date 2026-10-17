#!/bin/sh
# Usage: bench/load.sh [THREADS]
# Compares loading the `bench` database of the quiver-bench compose project
# back into bench_load: quiver load from quiver's SQL and CSV dumps, myloader
# from mydumper's dump and from quiver's. Same thread count; myloader keeps the
# binlog on (--enable-binlog), as quiver load does. Both add secondary keys
# after the data by default (myloader: --optimize-keys AFTER_IMPORT_PER_TABLE).
# Wall time and client CPU from hyperfine; results land in bench/results/<timestamp>-load-t<THREADS>/.
set -eu
threads=${1:-4}
root=$(cd "$(dirname "$0")/.." && pwd)
out=${BENCH_OUT:-${TMPDIR:-/tmp}/quiver-bench-load}
results="$root/bench/results/$(date +%Y%m%d-%H%M%S)-load-t$threads"
rm -rf "$out"
mkdir -p "$results" "$out"
switch=${QUIVER_SWITCH:-quiver-ox52}
(cd "$root" && opam exec --switch="$switch" -- dune build --build-dir _build_ox_release --profile release ./bin/main.exe)
quiver="$root/_build_ox_release/default/bin/main.exe"
connection="-P 33200 -p quiver"
sql() {
  docker compose -p quiver-bench -f "$root/bench/compose.yml" exec -T -e MYSQL_PWD=quiver mysql \
    mysql -uroot -N "$@"
}
sql -e "SET GLOBAL local_infile = 1"
# shellcheck disable=SC2086
"$quiver" dump $connection -B bench -t "$threads" -o "$out/sql" > /dev/null
# shellcheck disable=SC2086
"$quiver" dump $connection -B bench -t "$threads" --format csv --csv-dialect mysql-load-data \
  -o "$out/csv" > /dev/null
mydumper -h 127.0.0.1 -P 33200 -u root -p quiver -B bench -t "$threads" --complete-insert \
  --rows 1000000 -o "$out/mydumper" > /dev/null 2>&1
reset="docker compose -p quiver-bench -f $root/bench/compose.yml exec -T -e MYSQL_PWD=quiver mysql mysql -uroot -e 'DROP DATABASE IF EXISTS bench_load'"
myloader="myloader -h 127.0.0.1 -P 33200 -u root -p quiver -B bench_load -t $threads --enable-binlog"

hyperfine --runs "${RUNS:-3}" --prepare "$reset" \
  --export-markdown "$results/load.md" --export-json "$results/load.json" \
  -n "quiver load sql" "$quiver load $connection -B bench_load -t $threads -d $out/sql" \
  -n "quiver load csv" "$quiver load $connection -B bench_load -t $threads -d $out/csv" \
  -n "quiver load sql, keys with table" "$quiver load $connection -B bench_load -t $threads --secondary-keys with-table -d $out/sql" \
  -n "myloader (mydumper dump)" "$myloader -d $out/mydumper" \
  -n "myloader (quiver sql dump)" "$myloader -d $out/sql"

sizes() {
  du -sm "$1" | cut -f1
}
{
  echo "| dump | MiB |"
  echo "|---|---|"
  echo "| quiver sql | $(sizes "$out/sql") |"
  echo "| quiver csv | $(sizes "$out/csv") |"
  echo "| mydumper | $(sizes "$out/mydumper") |"
} > "$results/size.md"
cat "$results/load.md" "$results/size.md"
