type system = {
  getenv : string -> string option;
  read : string -> string option;
  entries : string -> string list;
  exists : string -> bool;
}

let system =
  {
    getenv = Sys.getenv_opt;
    read =
      (fun path ->
        match In_channel.with_open_bin path In_channel.input_all with
        | text -> Some text
        | exception Sys_error (_ : string) -> None);
    entries =
      (fun directory ->
        match Sys.readdir directory with
        | names -> Array.to_list names
        | exception Sys_error (_ : string) -> []);
    exists = Sys.file_exists;
  }

type error = No_socket of string list [@@deriving sexp_of]

let socket { getenv; read; entries; exists } =
  let set name =
    Option.bind (getenv name) (function "" -> None | value -> Some value)
  in
  let defaults =
    List.filter_map Fun.id
      [
        Some "/etc/my.cnf";
        Some "/etc/mysql/my.cnf";
        Some "/usr/local/etc/my.cnf";
        Some "/opt/homebrew/etc/my.cnf";
        Option.map
          (fun home -> Filename.concat home "my.cnf")
          (Base.Option.first_some (set "MARIADB_HOME") (set "MYSQL_HOME"));
        Option.map (fun home -> Filename.concat home ".my.cnf") (set "HOME");
      ]
  and packaged =
    [
      "/run/mysqld/mysqld.sock";
      "/var/run/mysqld/mysqld.sock";
      "/var/lib/mysql/mysql.sock";
      "/tmp/mysql.sock";
    ]
  in
  let rec file depth found path =
    match (depth, read path) with
    | depth, Some text when depth <= 10 ->
        fst
          (List.fold_left
             (fun (found, group) (line : Option_file.line) ->
               match line with
               | Group name -> (found, Some name)
               | Option { name = "socket"; value = Some path }
                 when Base.Option.exists group ~f:(fun group ->
                          List.mem group [ "client"; "client-server"; "client-mariadb" ])
                 ->
                   ((match path with "" -> found | path -> Some path), group)
               | Option { name = (_ : string); value = (_ : string option) } -> (found, group)
               | Include path -> (file (depth + 1) found path, group)
               | Include_dir directory ->
                   ( List.fold_left
                       (fun found name ->
                         file (depth + 1) found (Filename.concat directory name))
                       found
                       (List.sort String.compare
                          (List.filter
                             (fun name -> Filename.check_suffix name ".cnf")
                             (entries directory))),
                     group ))
             (found, None) (Option_file.lines text))
    | (_ : int), (Some (_ : string) | None) -> found
  in
  match
    Base.Option.first_some
      (List.fold_left (file 0) None defaults)
      (Base.Option.first_some (set "MYSQL_UNIX_PORT") (List.find_opt exists packaged))
  with
  | Some path -> Ok (File_path.of_string path)
  | None -> Error (`Local (No_socket (defaults @ ("MYSQL_UNIX_PORT" :: packaged))))

external send_space : unit -> int = "quiver_local_send_space" [@@noalloc]

let send_space () = match send_space () with -1 -> None | space -> Some space
