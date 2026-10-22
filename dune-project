(lang dune 3.17)


(name quiver)

(version 1.3.1)

(generate_opam_files true)

(authors "Gleb")

(maintainers "Gleb")

(license AGPL-3.0-or-later)

(source
 (github jongleb/quiver))

(package
 (name quiver)
 (synopsis "Parallel consistent logical database dumper")
 (description
  "An embeddable OCaml library and CLI for parallel, consistent logical dumps with per-DBMS providers.")
 (depends
  (ocaml
   (>= 5.2))
  conf-zstd
  conf-openssl
  conf-pkg-config
  dune-configurator
  base
  base_bigstring
  unboxed
  unique
  await
  core
  core_unix
  file_path
  stdio
  angstrom
  hex_encode
  ppx_sexp_conv
  ppx_compare
  ppx_hash
  ppx_yojson_conv
  ppx_let
  (ptime
   (and
    :with-test
    (>= 1.1)))
  (uuidm
   (>= 0.9))
  (yojson
   (>= 2.0))
  (cmdliner
   (>= 1.3))
  (bytesrw
   (and
    :with-test
    (>= 0.4)))
  (conf-xxhash :with-test)
  (alcotest :with-test)
  (qcheck-core :with-test)
  (qcheck-alcotest :with-test)
  (ppx_expect :with-test)))
