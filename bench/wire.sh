#!/bin/sh
# Usage: [RATE_KBPS=N] bench/wire.sh [RTT_MS|direct] [DATABASE] [RUNS]
# quiver agent -> toxiproxy (adds RTT_MS round trip) -> quiver collect writing
# plain files, or straight over loopback with "direct" (no proxy: what the
# hosts' CPUs cost, with instructions and CPU seconds of agent and collector
# from /usr/bin/time on macOS), for three shapes: as many streams and chunks in flight as
# readers (each reader waits for its commit), the defaults (twice the chunks),
# and four times the streams. Needs the test compose project (mysql84 and
# toxiproxy): docker compose -p quiver up -d --wait mysql84 toxiproxy.
# RATE_KBPS caps every connection, as one TCP flow is capped on a long path;
# AGENT_FLAGS go to every agent run (for example --chunk-bytes). WIRE=plain
# runs the protocol without TLS; by default both ends use mutual TLS with the
# test certificates.
# Prints wall seconds per run; results land in bench/results/<timestamp>-wire.
set -eu
rtt=${1:-100}
database=${2:-quiver_it}
runs=${3:-2}
root=$(cd "$(dirname "$0")/.." && pwd)
results="$root/bench/results/$(date +%Y%m%d-%H%M%S)-wire-${WIRE:-tls}-rtt$rtt"
out=${TMPDIR:-/tmp}/quiver-wire
mkdir -p "$results"
switch=${QUIVER_SWITCH:-quiver-ox52}
(cd "$root" && opam exec --switch="$switch" -- dune build --build-dir _build_ox_release --profile release ./bin/main.exe)
quiver="$root/_build_ox_release/default/bin/main.exe"
case ${WIRE:-tls} in
  plain) wire="--wire-tls disabled" ;;
  *) wire="--wire-cert $root/test/tls/node.crt --wire-key $root/test/tls/node.key --wire-peers $root/test/tls/ca.crt" ;;
esac
api=http://127.0.0.1:8474
case $rtt in
  direct)
    collector_address=127.0.0.1:7600
    timed="/usr/bin/time -l"
    ;;
  *)
    collector_address=127.0.0.1:33300
    timed=
    half=$((rtt / 2))
    curl -fsS -X DELETE "$api/proxies/wire" > /dev/null 2>&1 || true
    curl -fsS -X POST "$api/proxies" \
      -d '{"name":"wire","listen":"0.0.0.0:33300","upstream":"host.docker.internal:7600"}' > /dev/null
    for stream in upstream downstream; do
      curl -fsS -X POST "$api/proxies/wire/toxics" \
        -d "{\"name\":\"delay-$stream\",\"type\":\"latency\",\"stream\":\"$stream\",\"attributes\":{\"latency\":$half}}" > /dev/null
    done
    if [ -n "${RATE_KBPS:-}" ]; then
      curl -fsS -X POST "$api/proxies/wire/toxics" \
        -d "{\"name\":\"rate\",\"type\":\"bandwidth\",\"stream\":\"upstream\",\"attributes\":{\"rate\":$RATE_KBPS}}" > /dev/null
    fi
    ;;
esac
cost() {
  awk '/instructions retired/ { i = $1 } / real / { u = $3; s = $5 } END { if (i) printf "%.2fG instr, %ss user, %ss sys", i / 1e9, u, s }' "$1"
}
echo "| shape | run | seconds | agent | collector |" > "$results/wire.md"
echo "|---|---|---|---|---|" >> "$results/wire.md"
for shape in "-t 4 --streams 4 --chunks-in-flight 4" "-t 4" "-t 4 --streams 16 --chunks-in-flight 12"; do
  run=1
  while [ "$run" -le "$runs" ]; do
    rm -rf "$out"
    # shellcheck disable=SC2086
    $timed "$quiver" collect $wire --listen 127.0.0.1:7600 -o "$out" > "$results/collect.log" 2>&1 &
    collector=$!
    sleep 1
    started=$(python3 -c "import time; print(time.time())")
    # shellcheck disable=SC2086
    $timed "$quiver" agent $wire -P 33084 -p quiver -B "$database" $shape --zstd-level 1 ${AGENT_FLAGS:-} \
      --collector "$collector_address" > "$results/agent.log" 2>&1
    wait $collector
    seconds=$(python3 -c "import time; print(round(time.time() - $started, 2))")
    echo "| $shape | $run | $seconds | $(cost "$results/agent.log") | $(cost "$results/collect.log") |" \
      | tee -a "$results/wire.md"
    run=$((run + 1))
  done
done
[ "$rtt" = direct ] || curl -fsS -X DELETE "$api/proxies/wire" > /dev/null
