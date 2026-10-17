#!/bin/sh
# Usage: bench/run.sh [THREADS]
# Compares quiver and mydumper on the `bench` database of the quiver-bench
# compose project (bench/compose.yml, bench/generate.sql) with the same number
# of threads and FTWRL consistency, plain SQL and zstd: wall time, client CPU
# and peak memory (hyperfine), output size and quiver's allocations per row
# (from the manifest). Results
# land in bench/results/<timestamp>-t<THREADS>/.
set -eu
threads=${1:-4}
root=$(cd "$(dirname "$0")/.." && pwd)
out=${BENCH_OUT:-${TMPDIR:-/tmp}/quiver-bench}
results="$root/bench/results/$(date +%Y%m%d-%H%M%S)-t$threads"
mkdir -p "$results"
# Release builds use a flambda switch (-O3 in the release profile, see /dune).
switch=${QUIVER_SWITCH:-quiver-ox52}
(cd "$root" && opam exec --switch="$switch" -- dune build --build-dir _build_ox_release --profile release ./bin/main.exe)
quiver="$root/_build_ox_release/default/bin/main.exe dump -P 33200 -p quiver -B bench -t $threads"
mydumper="mydumper -h 127.0.0.1 -P 33200 -u root -p quiver -B bench -t $threads --complete-insert --rows 1000000 --sync-thread-lock-mode FTWRL"

hyperfine --runs "${RUNS:-3}" --prepare "rm -rf $out && mkdir -p $out" \
  --export-markdown "$results/time.md" --export-json "$results/time.json" \
  -n "quiver sql" "$quiver -o $out/q" \
  -n "mydumper sql" "$mydumper -o $out/m" \
  -n "quiver zstd" "$quiver --compress zstd -o $out/q" \
  -n "mydumper zstd" "$mydumper -c zstd -o $out/m"

sizes() {
  rm -rf "$out" && mkdir -p "$out"
  "$@" > /dev/null
  du -sm "$out"/* | cut -f1
}
{
  echo "| output | MiB |"
  echo "|---|---|"
  # shellcheck disable=SC2086
  echo "| quiver sql | $(sizes $quiver -o "$out/q") |"
  # shellcheck disable=SC2086
  echo "| mydumper sql | $(sizes $mydumper -o "$out/m") |"
  # shellcheck disable=SC2086
  echo "| quiver zstd | $(sizes $quiver --compress zstd -o "$out/q") |"
  # shellcheck disable=SC2086
  echo "| mydumper zstd | $(sizes $mydumper -c zstd -o "$out/m") |"
} > "$results/size.md"

rm -rf "$out" && mkdir -p "$out"
# shellcheck disable=SC2086
$quiver -o "$out/q" > /dev/null
python3 - "$out/q/quiver.json" > "$results/allocation.md" <<'PY'
import json, sys
chunks = [c["state"][1] for c in json.load(open(sys.argv[1]))["chunks"]]
rows = sum(c["part"]["rows"] for c in chunks)
words = sum(c["allocated_words"] for c in chunks)
print(f"allocated {words} words for {rows} rows: {words / rows:.1f} words per row")
PY
cat "$results/allocation.md"
python3 - "$results/time.json" > "$results/summary.md" <<'PY'
import json, sys
print("| run | wall, s | client user CPU, s | client sys CPU, s | instructions, 10⁹ | peak RSS, MiB |")
print("|---|---|---|---|---|---|")
for r in json.load(open(sys.argv[1]))["results"]:
    m = r["summary"]
    print(f"| {r['name']} | {m['time_wall_clock']['mean']:.2f} ± {m['time_wall_clock']['stddev']:.2f}"
          f" | {m['time_user']['mean']:.2f} | {m['time_system']['mean']:.2f}"
          f" | {m['instructions']['mean'] / 1e9:.0f} | {m['memory_peak_resident']['mean'] / 1048576:.0f} |")
PY
cat "$results/summary.md"
