#!/bin/sh
# Usage: roundtrip.sh QUIVER LINK_CHECK DIFFERENTIAL COMPOSE_FILE FIXTURE BANK LOAD_STATEMENT SAKILA_SCHEMA SAKILA_DATA OUT NODE_CERT
# For every test server: load FIXTURE, dump it, drop it, restore the dump,
# and require identical, non-NULL CHECKSUM TABLE results and row counts.
# QUIVER_IT_SERVERS="mysql84 mariadb118" limits the run to those services.
set -eu
[ "${QUIVER_IT:-}" = 1 ] || exit 0
quiver=$1 link_check=$2 differential=$3 compose=$4 fixture=$5 bank=$6 load=$7 sakila_schema=$8 sakila_data=$9 out=${10}
# Every node of quiver's protocol speaks mutual TLS with the test certificates.
certs=$(cd "$(dirname "${11}")" && pwd)
wire="--wire-cert $certs/node.crt --wire-key $certs/node.key --wire-peers $certs/ca.crt"
# myloader of mydumper v1.0.11, pinned: restores the mydumper layout quiver writes.
myloader_image=mydumper/mydumper@sha256:7ac39f7dbcf381b3c560dc81e108e28f8aa82c0422fe97ffa7af75d296f5b494
tables="quiver_it.all_types, quiver_it.big, quiver_it.no_key, quiver_it.uniq, quiver_it.str_key, quiver_it.latin_key, quiver_it.bin_key, quiver_it.composite, quiver_it.date_key, quiver_it.nothing"
fail() { echo "$service: $*" >&2; exit 1; }
# Runs a command in the background and kills it after $1 seconds, so a node of
# quiver's protocol that hangs fails the step instead of the whole run; its
# process id is left in $last.
watched() {
  limit=$1; shift
  "$@" &
  last=$!
  (sleep "$limit"; kill "$last") > /dev/null 2>&1 &
}
selected() {
  case " ${QUIVER_IT_SERVERS:-$1} " in *" $1 "*) return 0 ;; *) return 1 ;; esac
}
# Restores the tables and data of a dump without myloader: the database
# first, the rest inside it (files carry no USE, as in mydumper's layout).
# Views, triggers and routines are myloader's (no DELIMITER); see objects.
# READ is cat or zstd -dcq.
restore() {
  dir=$1 database=$2 read=$3 extension=$4
  $read "$dir"/*-schema-create."$extension" | sql
  # shellcheck disable=SC2046
  $read $(ls "$dir"/*."$extension" | grep -v -e '-schema-create\.' -e '-schema-post\.' -e '-schema-triggers\.' -e '-schema-view\.') | sql -D "$database"
}
roundtrip() {
  selected "$1" || return 0
  service=$1 port=$2 client=$3 depth=$4 tls=$5 snapshot=$6 format=${7:-sql}
  sql() {
    docker compose -p quiver -f "$compose" exec -T -e MYSQL_PWD=quiver "$service" \
      "$client" -uroot --local-infile=1 --default-character-set=utf8mb4 "$@"
  }
  check() {
    sums=$(sql -N -e "CHECKSUM TABLE $tables")
    [ "$(printf '%s\n' "$sums" | grep -c -E '	[0-9]+$')" = 10 ] || fail "bad checksums: $sums"
    [ "$(sql -N -e 'SELECT COUNT(*) FROM quiver_it.big')" = 50000 ] || fail "big lost rows"
    printf '%s\n' "$sums"
  }
  sed "s/cte_max_recursion_depth/$depth/" "$fixture" | sql
  before=$(check)
  dir="$out/$service-$snapshot-$format"
  rm -rf "$dir"
  # Straight into the same server and back, with no files.
  if [ "$format" = copy ]; then
    copy() {
      "$quiver" copy --snapshot "$snapshot" --tls "$tls" -P "$port" -p quiver -B "$1" -t 4 \
        --chunk-bytes 200000 --to-tls "$tls" --to-port "$port" --to-password quiver \
        --to-database "$2" --secondary-keys after-data > "$dir.$2.log" 2>&1 \
        || fail "quiver copy: $(cat "$dir.$2.log")"
    }
    sql -e "DROP DATABASE IF EXISTS quiver_it_c"
    copy quiver_it quiver_it_c
    sql -e "DROP DATABASE quiver_it"
    copy quiver_it_c quiver_it
    sql -e "DROP DATABASE quiver_it_c"
    after=$(check)
    [ "$before" = "$after" ] || fail "checksum mismatch after copy: $before / $after"
    echo "$service $snapshot copy: checksums match"
    return 0
  fi
  # Through quiver's own protocol: an agent and a collector loading into the
  # same server, then a collector writing files that quiver load restores.
  if [ "$format" = net ]; then
    sql -e "DROP DATABASE IF EXISTS quiver_it_n"
    watched 600 "$quiver" collect $wire --listen 127.0.0.1:7390 --to-tls "$tls" --to-port "$port" \
      --to-password quiver --to-database quiver_it_n --secondary-keys after-data > "$dir.collect.log" 2>&1
    collector=$last
    sleep 1
    watched 600 "$quiver" agent $wire --snapshot "$snapshot" --tls "$tls" -P "$port" -p quiver -B quiver_it \
      -t 4 --chunk-bytes 200000 --collector 127.0.0.1:7390 > "$dir.agent.log" 2>&1
    wait "$last" || fail "quiver agent: $(cat "$dir.agent.log") collector: $(cat "$dir.collect.log")"
    wait $collector || fail "quiver collect: $(cat "$dir.collect.log")"
    watched 600 "$quiver" collect $wire --listen 127.0.0.1:7391 -o "$dir" --compress zstd \
      > "$dir.collect-files.log" 2>&1
    collector=$last
    sleep 1
    watched 600 "$quiver" agent $wire --snapshot "$snapshot" --tls "$tls" -P "$port" -p quiver -B quiver_it_n \
      -t 4 --chunk-bytes 200000 --collector 127.0.0.1:7391 > "$dir.agent-files.log" 2>&1
    wait "$last" || fail "quiver agent: $(cat "$dir.agent-files.log") collector: $(cat "$dir.collect-files.log")"
    wait $collector || fail "quiver collect: $(cat "$dir.collect-files.log")"
    sql -e "DROP DATABASE quiver_it; DROP DATABASE quiver_it_n"
    "$quiver" load --tls "$tls" -P "$port" -p quiver -d "$dir" -B quiver_it -t 4 > "$dir.load.log" 2>&1 \
      || fail "quiver load: $(cat "$dir.load.log")"
    after=$(check)
    [ "$before" = "$after" ] || fail "checksum mismatch through the wire: $before / $after"
    echo "$service $snapshot net: checksums match (to a server, to files)"
    return 0
  fi
  # Through two relays into a server, then one read of the source reaching a
  # chain of three targets: files, a server, CSV files.
  if [ "$format" = hops ]; then
    sql -e "DROP DATABASE IF EXISTS quiver_it_r; DROP DATABASE IF EXISTS quiver_it_b"
    background() { log=$1; shift; watched 600 "$quiver" "$@" > "$dir.$log.log" 2>&1; eval "pid_$log=\$last"; }
    finished() { eval "wait \$pid_$1" || fail "$1: $(cat "$dir.$1.log")"; }
    background last collect $wire --listen 127.0.0.1:7393 --to-tls "$tls" --to-port "$port" --to-password quiver \
      --to-database quiver_it_r
    background second relay $wire --listen 127.0.0.1:7394 --forward 127.0.0.1:7393
    background first relay $wire --listen 127.0.0.1:7395 --forward 127.0.0.1:7394
    sleep 1
    watched 600 "$quiver" agent $wire --snapshot "$snapshot" --tls "$tls" -P "$port" -p quiver -B quiver_it \
      -t 4 --chunk-bytes 200000 --collector 127.0.0.1:7395 > "$dir.agent.log" 2>&1
    wait "$last" || fail "quiver agent: $(cat "$dir.agent.log")"
    finished first; finished second; finished last
    rm -rf "$dir-a" "$dir-c"
    background c collect $wire --listen 127.0.0.1:7396 -o "$dir-c" --format csv --csv-dialect mysql-load-data --compress zstd
    sleep 0.5
    background b collect $wire --listen 127.0.0.1:7397 --forward 127.0.0.1:7396 --to-tls "$tls" --to-port "$port" \
      --to-password quiver --to-database quiver_it_b --secondary-keys after-data
    sleep 0.5
    background a collect $wire --listen 127.0.0.1:7398 --forward 127.0.0.1:7397 -o "$dir-a"
    sleep 1
    watched 600 "$quiver" agent $wire --snapshot "$snapshot" --tls "$tls" -P "$port" -p quiver -B quiver_it \
      -t 4 --chunk-bytes 200000 --collector 127.0.0.1:7398 > "$dir.agent-chain.log" 2>&1
    wait "$last" || fail "quiver agent: $(cat "$dir.agent-chain.log")"
    finished a; finished b; finished c
    copies() {
      sql -N -e "CHECKSUM TABLE $(echo "$tables" | sed "s/quiver_it\./$1./g")" | sed "s/^$1\./quiver_it./"
    }
    [ "$(copies quiver_it_r)" = "$before" ] || fail "checksums differ after two relays"
    [ "$(copies quiver_it_b)" = "$before" ] || fail "checksums differ at the chain's server"
    sql -e "DROP DATABASE quiver_it; DROP DATABASE quiver_it_r; DROP DATABASE quiver_it_b"
    "$quiver" load --tls "$tls" -P "$port" -p quiver -d "$dir-a" -t 4 > "$dir.load-a.log" 2>&1 \
      || fail "quiver load of the chain's first files: $(cat "$dir.load-a.log")"
    [ "$(check)" = "$before" ] || fail "checksums differ in the chain's first files"
    sql -e "DROP DATABASE quiver_it"
    "$quiver" load --tls "$tls" -P "$port" -p quiver -d "$dir-c" -t 4 > "$dir.load-c.log" 2>&1 \
      || fail "quiver load of the chain's CSV files: $(cat "$dir.load-c.log")"
    after=$(check)
    [ "$before" = "$after" ] || fail "checksums differ in the chain's CSV files"
    echo "$service $snapshot hops: two relays and a chain of three targets match"
    return 0
  fi
  case $format in
    sql) flags="--format sql" ;;
    sql-zstd) flags="--format sql --compress zstd --zstd-level 9" ;;
    csv-rfc4180) flags="--format csv --csv-dialect rfc4180" ;;
    csv-mysql-load-data) flags="--format csv --csv-dialect mysql-load-data" ;;
    csv-mysql-load-data-zstd)
      flags="--format csv --csv-dialect mysql-load-data --compress zstd --zstd-level 3" ;;
  esac
  # shellcheck disable=SC2086
  "$quiver" dump $flags --snapshot "$snapshot" --tls "$tls" -P "$port" -p quiver -B quiver_it -t 4 \
    --chunk-bytes 200000 -o "$dir" > "$dir.log"
  case $snapshot in
    ftwrl) grep -q "^Global_read_lock held for" "$dir.log" || fail "no FTWRL report" ;;
    backup-stage) grep -q "^Backup_stage_block_commit held for" "$dir.log" || fail "no backup stage report" ;;
    trx-only) grep -q "^no lock" "$dir.log" || fail "no trx-only report" ;;
  esac
  sql -e "DROP DATABASE quiver_it"
  case $format in
    sql)
      for table in all_types big no_key uniq str_key latin_key bin_key composite date_key nothing; do
        ls "$dir/quiver_it.$table.00000.sql" >/dev/null || fail "no data file for $table"
      done
      for table in big str_key latin_key bin_key composite date_key; do
        [ "$(ls "$dir"/quiver_it."$table".*.sql | wc -l)" -gt 1 ] || fail "$table was not split"
      done
      restore "$dir" quiver_it cat sql ;;
    sql-zstd)
      zstd -tq "$dir"/*.zst || fail "corrupt zstd output"
      "$quiver" load --tls "$tls" -P "$port" -p quiver -d "$dir" -t 4 > "$dir.load.log" 2>&1 \
        || fail "quiver load: $(cat "$dir.load.log")" ;;
    csv-mysql-load-data)
      restore "$dir" quiver_it cat sql
      for table in all_types big no_key uniq str_key latin_key bin_key composite date_key nothing; do
        statement=$( (echo "SET @t = '$table', @dialect = 'mysql-load-data';"; cat "$load") | sql -N --raw)
        # An empty stdin would leave docker exec waiting; skip tables without rows.
        if [ "$(cat "$dir"/quiver_it."$table".*.csv | wc -c)" -gt 0 ]; then
          cat "$dir"/quiver_it."$table".*.csv | sql -e "$statement" \
            || fail "LOAD DATA failed for $table"
        fi
      done ;;
    csv-rfc4180|csv-mysql-load-data-zstd)
      case $format in
        *-zstd) zstd -tq "$dir"/*.zst || fail "corrupt zstd output" ;;
      esac
      "$quiver" load --tls "$tls" -P "$port" -p quiver -d "$dir" -t 4 > "$dir.load.log" 2>&1 \
        || fail "quiver load: $(cat "$dir.load.log")" ;;
  esac
  after=$(check)
  [ "$before" = "$after" ] || fail "checksum mismatch: $before / $after"
  echo "$service $snapshot $format: checksums match"
}
rejects() {
  selected "$1" || return 0
  service=$1 port=$2 snapshot=$3 expected=$4
  if "$quiver" dump --snapshot "$snapshot" -P "$port" -p quiver -B quiver_it -o "$out/rejected" 2> "$out/rejected.log"; then
    fail "$snapshot was accepted"
  fi
  grep -q "$expected" "$out/rejected.log" || fail "$snapshot failed with: $(cat "$out/rejected.log")"
  echo "$service $snapshot: rejected"
}
# Connects with quiver's own protocol client, runs queries and expects a
# wrong password to be refused.
linked() {
  service=$1 port=$2 tls=$3
  selected "$1" || return 0
  result=$("$link_check" "$port" "$tls") || fail "own client: $result"
  printf '%s\n' "$result" | grep -qx "1|NULL|x'y|1" || fail "own client rows: $result"
  printf '%s\n' "$result" | grep -q "(code 1054)" || fail "own client error: $result"
  printf '%s\n' "$result" | grep -q "wrong password: (Refused (code 1045)" || fail "own client auth: $result"
  echo "$service: own client connects ($tls)"
}
# Over the server's Unix socket, mounted on the host (Linux only: Docker on
# macOS shows the file but does not carry its connections): a dump with the
# password in the clear after FLUSH PRIVILEGES empties caching_sha2's cache,
# quiver load back, localhost found through MYSQL_UNIX_PORT, and on MariaDB a
# user that unix_socket lets in by the uid of the process, without a password.
sockets() {
  service=$1 client=$2
  selected "$1" || return 0
  [ "$(uname -s)" = Linux ] || { echo "$service: sockets skipped (not Linux)"; return 0; }
  socket=${QUIVER_IT_SOCKETS:-/tmp/quiver-it-sockets}/$service/mysqld.sock
  sql() {
    docker compose -p quiver -f "$compose" exec -T -e MYSQL_PWD=quiver "$service" \
      "$client" -uroot --default-character-set=utf8mb4 "$@"
  }
  sums() { sql -N -e "CHECKSUM TABLE $tables"; }
  sed "s/cte_max_recursion_depth/${3:-cte_max_recursion_depth}/" "$fixture" | sql
  before=$(sums)
  dir="$out/$service-socket"
  rm -rf "$dir"
  sql -e "FLUSH PRIVILEGES"
  "$quiver" dump -S "$socket" -p quiver -B quiver_it -t 4 --chunk-bytes 200000 \
    --compress zstd -o "$dir" > "$dir.log" 2>&1 || fail "dump over the socket: $(cat "$dir.log")"
  sql -e "DROP DATABASE quiver_it"
  "$quiver" load -S "$socket" -p quiver -d "$dir" -t 4 > "$dir.load.log" 2>&1 \
    || fail "load over the socket: $(cat "$dir.load.log")"
  [ "$(sums)" = "$before" ] || fail "checksums differ after a dump and load over the socket"
  MYSQL_UNIX_PORT=$socket "$quiver" dump -h localhost -p quiver -B quiver_it -t 2 -o "$dir-localhost" \
    > "$dir-localhost.log" 2>&1 || fail "localhost through MYSQL_UNIX_PORT: $(cat "$dir-localhost.log")"
  if [ "$client" = mariadb ]; then
    uid=$(id -u)
    docker compose -p quiver -f "$compose" exec -T "$service" sh -c \
      "getent passwd $uid > /dev/null || useradd -u $uid quiverit"
    name=$(docker compose -p quiver -f "$compose" exec -T "$service" getent passwd "$uid" | cut -d: -f1)
    sql -e "CREATE USER IF NOT EXISTS '$name'@'localhost' IDENTIFIED VIA unix_socket; \
      GRANT ALL ON *.* TO '$name'@'localhost'"
    "$quiver" dump -S "$socket" -u "$name" -B quiver_it -t 2 -o "$dir-peer" > "$dir-peer.log" 2>&1 \
      || fail "unix_socket as $name: $(cat "$dir-peer.log")"
  fi
  echo "$service: dump and load over the Unix socket match"
}
# Loads sakila (views, triggers, routines, GEOMETRY, FULLTEXT) plus an event,
# dumps it, restores it with myloader as sakila_q, and requires identical
# table checksums and object definitions (sql_mode and time zone included;
# "; \n" for ";\n" is the only change myloader's statement split needs).
objects() {
  selected "$1" || return 0
  service=$1 port=$2 client=$3 tls=$4
  sql() {
    docker compose -p quiver -f "$compose" exec -T -e MYSQL_PWD=quiver "$service" \
      "$client" -uroot --default-character-set=utf8mb4 "$@"
  }
  sql -e "DROP DATABASE IF EXISTS sakila; DROP DATABASE IF EXISTS sakila_q; DROP DATABASE IF EXISTS sakila_l; DROP DATABASE IF EXISTS sakila_bad; DROP DATABASE IF EXISTS sakila_c"
  sql < "$sakila_schema"
  sql < "$sakila_data"
  sql -e "CREATE EVENT sakila.touch ON SCHEDULE EVERY 1 DAY DISABLE DO SELECT 1"
  dir="$out/$service-sakila"
  rm -rf "$dir"
  "$quiver" dump --tls "$tls" -P "$port" -p quiver -B sakila -t 4 -o "$dir" > "$dir.log"
  docker run --rm --network quiver_default -v "$dir":/in "$myloader_image" \
    myloader -h "$service" -P 3306 -u root -p quiver -d /in -B sakila_q -t 4 > "$dir.myloader.log" 2>&1 \
    || fail "myloader failed: $(tail -3 "$dir.myloader.log")"
  ! grep -q CRITICAL "$dir.myloader.log" || fail "myloader: $(grep CRITICAL "$dir.myloader.log" | head -1)"
  sakila_tables=$(sql -N -e "SELECT GROUP_CONCAT(table_name ORDER BY table_name) FROM information_schema.tables WHERE table_schema = 'sakila' AND table_type = 'BASE TABLE'")
  sums() { sql -N -e "CHECKSUM TABLE $(echo "$sakila_tables" | sed "s/\([^,]*\)/$1.\1/g")" | sed "s/^$1\.//"; }
  [ "$(sums sakila)" = "$(sums sakila_q)" ] || fail "sakila table checksums differ"
  definitions() {
    sql -N -e "SELECT 'view', table_name, md5(REPLACE(view_definition, CONCAT(CHAR(96), '$1', CHAR(96), '.'), '')) FROM information_schema.views WHERE table_schema = '$1' UNION ALL SELECT 'trigger', trigger_name, md5(CONCAT(REPLACE(action_statement, '; \\n', ';\\n'), sql_mode)) FROM information_schema.triggers WHERE trigger_schema = '$1' UNION ALL SELECT routine_type, routine_name, md5(CONCAT(REPLACE(routine_definition, '; \\n', ';\\n'), sql_mode)) FROM information_schema.routines WHERE routine_schema = '$1' UNION ALL SELECT 'event', event_name, md5(CONCAT(event_definition, sql_mode, time_zone)) FROM information_schema.events WHERE event_schema = '$1' ORDER BY 1, 2"
  }
  [ "$(definitions sakila)" = "$(definitions sakila_q)" ] || fail "sakila object definitions differ"
  echo "$service sakila: tables and $(definitions sakila | wc -l | tr -d ' ') objects restored by myloader"
  "$quiver" load --tls "$tls" -P "$port" -p quiver -d "$dir" -B sakila_l -t 4 > "$dir.load.log" 2>&1 \
    || fail "quiver load: $(cat "$dir.load.log")"
  [ "$(sums sakila)" = "$(sums sakila_l)" ] || fail "sakila table checksums differ after quiver load"
  [ "$(definitions sakila)" = "$(definitions sakila_l)" ] || fail "sakila object definitions differ after quiver load"
  creates() {
    for table in $(echo "$sakila_tables" | tr ',' ' '); do
      sql -N -e "SHOW CREATE TABLE $1.$table" | cut -f2-
    done
  }
  creates sakila > "$dir.creates"
  creates sakila_l > "$dir.creates-loaded"
  diff "$dir.creates" "$dir.creates-loaded" > /dev/null \
    || fail "sakila table definitions differ after quiver load: $(diff "$dir.creates" "$dir.creates-loaded" | head -5)"
  echo "$service sakila: tables (with keys added after the data), data and objects restored by quiver load"
  rm -rf "$dir-bad"
  cp -R "$dir" "$dir-bad"
  bad=$(ls "$dir-bad"/sakila.rental.*.sql | head -1)
  sed -i.orig '$ s/)/ )/' "$bad" && rm -f "$bad.orig"
  if "$quiver" load --tls "$tls" -P "$port" -p quiver -d "$dir-bad" -B sakila_bad -t 4 > "$dir-bad.log" 2>&1; then
    fail "quiver load accepted a corrupt file"
  fi
  grep -q "Corrupt" "$dir-bad.log" || fail "corrupt file: $(cat "$dir-bad.log")"
  echo "$service sakila: a corrupt data file is refused"
  # The failed load left files committed; with the file repaired, a second
  # run resumes: it skips them, tolerates what exists and loads the rest.
  cp "$dir/$(basename "$bad")" "$bad"
  "$quiver" load --tls "$tls" -P "$port" -p quiver -d "$dir-bad" -B sakila_bad -t 4 > "$dir-resume.log" 2>&1 \
    || fail "resume: $(cat "$dir-resume.log")"
  [ "$(sums sakila)" = "$(sums sakila_bad)" ] || fail "sakila table checksums differ after resuming"
  [ "$(definitions sakila)" = "$(definitions sakila_bad)" ] || fail "sakila object definitions differ after resuming"
  [ "$(sql -N -e "SELECT COUNT(*) FROM quiver_load.progress")" = 0 ] || fail "resume left progress behind"
  echo "$service sakila: an interrupted load resumes ($(grep -o 'resumed: [0-9]*' "$dir-resume.log" || echo 'nothing') files kept)"
  "$quiver" copy --tls "$tls" -P "$port" -p quiver -B sakila -t 4 --to-tls "$tls" --to-port "$port" \
    --to-password quiver --to-database sakila_c --secondary-keys after-data > "$dir.copy.log" 2>&1 \
    || fail "quiver copy: $(cat "$dir.copy.log")"
  [ "$(sums sakila)" = "$(sums sakila_c)" ] || fail "sakila table checksums differ after quiver copy"
  [ "$(definitions sakila)" = "$(definitions sakila_c)" ] || fail "sakila object definitions differ after quiver copy"
  creates sakila_c > "$dir.creates-copied"
  diff "$dir.creates" "$dir.creates-copied" > /dev/null \
    || fail "sakila table definitions differ after quiver copy: $(diff "$dir.creates" "$dir.creates-copied" | head -5)"
  echo "$service sakila: tables, data and objects copied server to server"
  sql -e "DROP DATABASE sakila_c"
  watched 600 "$quiver" collect $wire --listen 127.0.0.1:7392 --to-tls "$tls" --to-port "$port" \
    --to-password quiver --to-database sakila_c --secondary-keys after-data > "$dir.collect.log" 2>&1
  collector=$last
  sleep 1
  watched 600 "$quiver" agent $wire --tls "$tls" -P "$port" -p quiver -B sakila -t 4 \
    --collector 127.0.0.1:7392 > "$dir.agent.log" 2>&1
  wait "$last" || fail "quiver agent: $(cat "$dir.agent.log") collector: $(cat "$dir.collect.log")"
  wait $collector || fail "quiver collect: $(cat "$dir.collect.log")"
  [ "$(sums sakila)" = "$(sums sakila_c)" ] || fail "sakila table checksums differ through the wire"
  [ "$(definitions sakila)" = "$(definitions sakila_c)" ] || fail "sakila object definitions differ through the wire"
  creates sakila_c > "$dir.creates-sent"
  diff "$dir.creates" "$dir.creates-sent" > /dev/null || fail "sakila table definitions differ through the wire"
  echo "$service sakila: tables, data and objects sent by an agent to a collector"
}
# Faults in the middle of a dump through toxiproxy (downstream slowed to
# 256 KB/s so the dump lasts): a reset connection, a silent one, sessions
# killed by the server, and a full disk. Each must end in a reported error,
# a non-zero exit, no uncaught exception and no quiver.json.
chaos() {
  selected "$1" || return 0
  service=$1 port=$2 client=$3 tls=$4
  sql() {
    docker compose -p quiver -f "$compose" exec -T -e MYSQL_PWD=quiver "$service" \
      "$client" -uroot --default-character-set=utf8mb4 "$@"
  }
  api=http://127.0.0.1:8474
  proxy() {
    curl -fsS -X DELETE "$api/proxies/quiver" > /dev/null 2>&1 || true
    curl -fsS -X POST "$api/proxies" \
      -d "{\"name\":\"quiver\",\"listen\":\"0.0.0.0:33300\",\"upstream\":\"$service:3306\"}" > /dev/null
    curl -fsS -X POST "$api/proxies/quiver/toxics" \
      -d '{"name":"slow","type":"bandwidth","stream":"downstream","attributes":{"rate":256}}' > /dev/null
  }
  toxic() {
    curl -fsS -X POST "$api/proxies/quiver/toxics" \
      -d "{\"name\":\"$1\",\"type\":\"$1\",\"stream\":\"downstream\",\"attributes\":{\"timeout\":0}}" > /dev/null
  }
  kill_sessions() {
    for id in $(sql -N -e "SELECT ID FROM information_schema.PROCESSLIST WHERE USER = 'root' AND ID <> CONNECTION_ID() AND COMMAND <> 'Daemon'"); do
      sql -e "KILL $id" || true
    done
  }
  failed() {
    name=$1 dir=$2
    ! grep -q "uncaught exception" "$dir.log" || fail "chaos $name: $(cat "$dir.log")"
    grep -q "^quiver: " "$dir.log" || fail "chaos $name: no error reported"
    [ ! -e "$dir/quiver.json" ] || fail "chaos $name: manifest written"
    echo "$service chaos $name: reported $(grep -o -m1 -E 'Timed_out|No space left|Wire Closed|Tls|Server \{|Io \{' "$dir.log" || echo error)"
  }
  interrupted() {
    name=$1; shift
    dir="$out/$service-chaos-$name"
    rm -rf "$dir"
    proxy
    "$quiver" dump --tls "$tls" -P 33300 -p quiver -B quiver_it -t 4 --chunk-bytes 200000 \
      --net-timeout 5 -o "$dir" > "$dir.log" 2>&1 &
    pid=$!
    sleep 2
    "$@"
    if wait "$pid"; then fail "chaos $name: the dump succeeded"; fi
    failed "$name" "$dir"
  }
  interrupted reset toxic reset_peer
  interrupted silence toxic timeout
  interrupted kill kill_sessions
  curl -fsS -X DELETE "$api/proxies/quiver" > /dev/null
  if command -v hdiutil > /dev/null; then
    device=$(hdiutil attach -nomount ram://4096 | tr -d ' \t')
    diskutil erasevolume HFS+ quiverfull "$device" > /dev/null
    dir=/Volumes/quiverfull/dump
    if "$quiver" dump --tls "$tls" -P "$port" -p quiver -B quiver_it -t 4 -o "$dir" > "$out/$service-chaos-disk.log" 2>&1; then
      hdiutil detach "$device" > /dev/null
      fail "chaos disk: the dump succeeded"
    fi
    [ ! -e "$dir/quiver.json" ] || { hdiutil detach "$device" > /dev/null; fail "chaos disk: manifest written"; }
    hdiutil detach "$device" > /dev/null
    failed disk "$out/$service-chaos-disk"
  fi
}
# TLS by authority: the server's certificate pinned by fingerprint, a wrong
# fingerprint and a foreign CA refused; on MySQL, which reloads TLS live, a CA
# of our own with a 127.0.0.1 certificate accepted, and refused by the name
# localhost that the certificate lacks.
certificates() {
  selected "$1" || return 0
  service=$1 port=$2 client=$3
  sql() {
    docker compose -p quiver -f "$compose" exec -T -e MYSQL_PWD=quiver "$service" \
      "$client" -uroot -N "$@"
  }
  tls_dir="$out/$service-tls"
  rm -rf "$tls_dir"
  mkdir -p "$tls_dir"
  sql -e "DROP DATABASE IF EXISTS quiver_tls; CREATE DATABASE quiver_tls; CREATE TABLE quiver_tls.t (id INT PRIMARY KEY); INSERT INTO quiver_tls.t VALUES (1)"
  tls_dump() {
    name=$1; shift
    "$quiver" dump -P "$port" -p quiver -B quiver_tls -o "$tls_dir/$name" "$@" > "$tls_dir/$name.log" 2>&1
  }
  refused() {
    name=$1; shift
    if tls_dump "$name" "$@"; then fail "tls $name: accepted"; fi
    grep -q Untrusted_certificate "$tls_dir/$name.log" || fail "tls $name: $(cat "$tls_dir/$name.log")"
  }
  fingerprint=$(openssl s_client -connect "127.0.0.1:$port" -starttls mysql < /dev/null 2> /dev/null \
    | openssl x509 -noout -fingerprint -sha256 | cut -d= -f2)
  [ -n "$fingerprint" ] || fail "tls: no server certificate"
  tls_dump pinned --tls-fingerprint "$fingerprint" || fail "tls pinned: $(cat "$tls_dir/pinned.log")"
  refused wrong-pin --tls-fingerprint "$(printf '00%.0s' $(seq 32))"
  openssl req -x509 -newkey rsa:2048 -nodes -days 2 -subj "/CN=quiver test CA" \
    -keyout "$tls_dir/ca-key.pem" -out "$tls_dir/ca.pem" 2> /dev/null
  refused foreign-ca --tls-ca "$tls_dir/ca.pem"
  case $service in
    mysql*)
      openssl req -newkey rsa:2048 -nodes -subj "/CN=quiver test server" \
        -keyout "$tls_dir/server-key.pem" -out "$tls_dir/server.csr" 2> /dev/null
      printf 'subjectAltName=IP:127.0.0.1\n' > "$tls_dir/san.ext"
      openssl x509 -req -in "$tls_dir/server.csr" -CA "$tls_dir/ca.pem" -CAkey "$tls_dir/ca-key.pem" \
        -CAcreateserial -days 2 -extfile "$tls_dir/san.ext" -out "$tls_dir/server-cert.pem" 2> /dev/null
      chmod 644 "$tls_dir"/*.pem
      for file in ca.pem server-cert.pem server-key.pem; do
        docker compose -p quiver -f "$compose" cp "$tls_dir/$file" "$service:/tmp/quiver-$file" > /dev/null 2>&1
      done
      previous=$(sql -e "SELECT CONCAT_WS(' ', @@GLOBAL.ssl_ca, @@GLOBAL.ssl_cert, @@GLOBAL.ssl_key)")
      sql -e "SET GLOBAL ssl_ca = '/tmp/quiver-ca.pem', ssl_cert = '/tmp/quiver-server-cert.pem', ssl_key = '/tmp/quiver-server-key.pem'; ALTER INSTANCE RELOAD TLS"
      own=ok
      tls_dump own-ca --tls-ca "$tls_dir/ca.pem" || own="$(cat "$tls_dir/own-ca.log")"
      if tls_dump own-ca-by-name -h localhost --tls-ca "$tls_dir/ca.pem"; then own="accepted the name localhost"; fi
      # shellcheck disable=SC2086
      set -- $previous
      sql -e "SET GLOBAL ssl_ca = '$1', ssl_cert = '$2', ssl_key = '$3'; ALTER INSTANCE RELOAD TLS"
      [ "$own" = ok ] || fail "tls own CA: $own"
      grep -q Untrusted_certificate "$tls_dir/own-ca-by-name.log" || fail "tls by name: $(cat "$tls_dir/own-ca-by-name.log")"
      echo "$service: TLS pinned, own CA (and its name check), wrong pin and foreign CA refused" ;;
    *) echo "$service: TLS pinned, wrong pin and foreign CA refused" ;;
  esac
}
# Dumps quiver_bank while transfers commit and requires the restored copy to
# hold both invariants; a dump that mixes points in time breaks them.
consistent() {
  selected "$1" || return 0
  service=$1 port=$2 client=$3 depth=$4 tls=$5 snapshot=$6
  sql() {
    docker compose -p quiver -f "$compose" exec -T -e MYSQL_PWD=quiver "$service" \
      "$client" -uroot --default-character-set=utf8mb4 "$@"
  }
  sed "s/cte_max_recursion_depth/$depth/" "$bank" | sql
  sql -e "CALL quiver_bank.transfer_until_stopped()" &
  churn=$!
  sleep 2
  dir="$out/$service-bank-$snapshot"
  rm -rf "$dir"
  "$quiver" dump --snapshot "$snapshot" --tls "$tls" -P "$port" -p quiver -B quiver_bank -t 8 \
    --chunk-bytes 20000 -o "$dir" > "$dir.log"
  sleep 1
  sql -e "UPDATE quiver_bank.churn SET running = 0"
  wait "$churn"
  [ "$(sql -N -e 'SELECT COUNT(*) FROM quiver_bank.ledger')" -gt 0 ] || fail "no transfers happened"
  sql -e "DROP DATABASE quiver_bank"
  restore "$dir" quiver_bank cat sql
  total=$(sql -N -e "SELECT SUM(balance) FROM quiver_bank.accounts")
  versions=$(sql -N -e "SELECT SUM(version) FROM quiver_bank.accounts")
  transfers=$(sql -N -e "SELECT COUNT(*) FROM quiver_bank.ledger")
  [ "$transfers" -gt 0 ] || fail "dump captured no transfers"
  [ "$total" = 20000000 ] || fail "$snapshot: balances sum to $total"
  [ "$versions" = $((2 * transfers)) ] || fail "$snapshot: $versions versions for $transfers transfers"
  echo "$service $snapshot: consistent under writes ($transfers transfers)"
}
# qcheck tables of every type with edge values, random keys and dump flags:
# insert, dump, restore with the server's client, compare rows and checksums.
# QUIVER_PBT_COUNT cases (50); QUIVER_PBT_SEED repeats a run.
random_tables() {
  selected "$1" || return 0
  service=$1 port=$2 client=$3 tls=$4 snapshots=$5
  mkdir -p "$out/$service-pbt"
  "$differential" "$quiver" "$compose" "$service" "$client" "$port" "$tls" "$snapshots" \
    "${QUIVER_PBT_COUNT:-50}" "$out/$service-pbt" > "$out/$service-pbt.log" 2>&1 \
    || fail "random tables: $(tr '\r' '\n' < "$out/$service-pbt.log" | grep -v '^.\[2K' | tail -40)"
  echo "$service: ${QUIVER_PBT_COUNT:-50} random tables restored unchanged"
}
mkdir -p "$out"
for service in mysql80 mysql84; do
  if selected "$service"; then
    docker compose -p quiver -f "$compose" exec -T -e MYSQL_PWD=quiver "$service" mysql -uroot -e "SET GLOBAL local_infile = 1"
  fi
done
sockets mysql84 mysql
sockets mariadb118 mariadb max_recursive_iterations
linked mysql80 33080 verified
linked mysql84 33084 verified
linked mariadb1011 33101 plain
linked mariadb114 33114 verified
linked mariadb118 33118 verified
certificates mysql80 33080 mysql
certificates mysql84 33084 mysql
certificates mariadb114 33114 mariadb
certificates mariadb118 33118 mariadb
objects mysql80 33080 mysql verified
objects mysql84 33084 mysql verified
objects mariadb1011 33101 mariadb disabled
objects mariadb114 33114 mariadb verified
objects mariadb118 33118 mariadb verified
random_tables mysql80 33080 mysql verified ftwrl,trx-only
random_tables mysql84 33084 mysql verified ftwrl,trx-only
random_tables mariadb1011 33101 mariadb disabled ftwrl,backup-stage,trx-only
random_tables mariadb114 33114 mariadb verified ftwrl,backup-stage,trx-only
random_tables mariadb118 33118 mariadb verified ftwrl,backup-stage,trx-only
roundtrip mysql80 33080 mysql cte_max_recursion_depth verified ftwrl
roundtrip mysql84 33084 mysql cte_max_recursion_depth verified ftwrl
roundtrip mysql84 33084 mysql cte_max_recursion_depth verified trx-only
rejects mysql84 33084 backup-stage Unsupported_snapshot_mode
roundtrip mariadb1011 33101 mariadb max_recursive_iterations disabled ftwrl
roundtrip mariadb1011 33101 mariadb max_recursive_iterations disabled backup-stage
roundtrip mariadb114 33114 mariadb max_recursive_iterations verified backup-stage
roundtrip mariadb118 33118 mariadb max_recursive_iterations verified ftwrl
roundtrip mariadb118 33118 mariadb max_recursive_iterations verified backup-stage
roundtrip mysql84 33084 mysql cte_max_recursion_depth verified ftwrl sql-zstd
roundtrip mariadb118 33118 mariadb max_recursive_iterations verified backup-stage sql-zstd
roundtrip mysql84 33084 mysql cte_max_recursion_depth verified ftwrl csv-mysql-load-data
roundtrip mysql84 33084 mysql cte_max_recursion_depth verified ftwrl csv-mysql-load-data-zstd
roundtrip mysql84 33084 mysql cte_max_recursion_depth verified ftwrl csv-rfc4180
roundtrip mariadb118 33118 mariadb max_recursive_iterations verified ftwrl csv-mysql-load-data
roundtrip mariadb118 33118 mariadb max_recursive_iterations verified ftwrl csv-rfc4180
roundtrip mysql84 33084 mysql cte_max_recursion_depth verified ftwrl copy
roundtrip mysql84 33084 mysql cte_max_recursion_depth verified ftwrl net
roundtrip mysql84 33084 mysql cte_max_recursion_depth verified ftwrl hops
roundtrip mariadb118 33118 mariadb max_recursive_iterations verified backup-stage net
roundtrip mariadb1011 33101 mariadb max_recursive_iterations disabled backup-stage copy
chaos mysql80 33080 mysql verified
chaos mysql84 33084 mysql verified
chaos mariadb1011 33101 mariadb disabled
chaos mariadb114 33114 mariadb verified
chaos mariadb118 33118 mariadb verified
consistent mysql80 33080 mysql cte_max_recursion_depth verified ftwrl
consistent mysql84 33084 mysql cte_max_recursion_depth verified ftwrl
consistent mariadb1011 33101 mariadb max_recursive_iterations disabled backup-stage
consistent mariadb118 33118 mariadb max_recursive_iterations verified backup-stage
consistent mariadb118 33118 mariadb max_recursive_iterations verified ftwrl
