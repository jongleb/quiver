(** How the package's own libraries end up in the binary. *)
type linking =
  | Shared  (** Whatever the linker picks, usually the shared library. *)
  | Static  (** The archive [libX.a] from the package's [libdir]: releases. *)

let () =
  let package = ref "" and fallback = ref [] and linking = ref Shared in
  Configurator.V1.main ~name:"quiver"
    ~args:
      [
        ("-package", Arg.Set_string package, "PKG pkg-config package");
        ( "-fallback",
          Arg.String (fun flag -> fallback := !fallback @ [ flag ]),
          "FLAG linker flag without pkg-config" );
        ( "-link",
          Arg.Symbol
            ( [ "shared"; "static" ],
              function "static" -> linking := Static | _ -> linking := Shared ),
          " link the package's libraries shared (default) or static" );
      ]
    (fun configurator ->
      let flags =
        match !linking with
        | Shared ->
            Option.value
              ~default:
                { Configurator.V1.Pkg_config.cflags = []; libs = !fallback }
              (Option.bind
                 (Configurator.V1.Pkg_config.get configurator)
                 (Configurator.V1.Pkg_config.query ~package:!package))
        | Static ->
            let pkg_config arguments =
              let program =
                Option.value ~default:"pkg-config"
                  (Configurator.V1.which configurator "pkg-config")
              in
              let run =
                Configurator.V1.Process.run configurator program
                  (arguments @ [ !package ])
              in
              if run.exit_code <> 0 then
                Configurator.V1.die "pkg-config %s %s: %s"
                  (String.concat " " arguments)
                  !package run.stderr
              else String.split_on_char ' ' (String.trim run.stdout)
              |> List.filter (fun word -> not (String.equal word ""))
            in
            let libdir = String.concat " " (pkg_config [ "--variable=libdir" ])
            and library flag =
              if String.starts_with ~prefix:"-l" flag then
                Some (String.sub flag 2 (String.length flag - 2))
              else None
            in
            let own = List.filter_map library (pkg_config [ "--libs" ]) in
            {
              cflags = pkg_config [ "--cflags" ];
              libs =
                List.map
                  (fun flag ->
                    let archive name =
                      Filename.concat libdir ("lib" ^ name ^ ".a")
                    in
                    match library flag with
                    | Some name
                      when List.mem name own && Sys.file_exists (archive name) ->
                        archive name
                    | Some _ | None -> flag)
                  (pkg_config [ "--static"; "--libs" ]);
            }
      in
      Configurator.V1.Flags.write_sexp "c_flags.sexp" flags.cflags;
      Configurator.V1.Flags.write_sexp "c_library_flags.sexp" flags.libs)
