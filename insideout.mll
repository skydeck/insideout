{
type token = Var of string | Text of string
}

let blank = [' ' '\t']
let ident = ['a'-'z']['a'-'z' '_' 'A'-'Z' '0'-'9']*

rule tokens = parse
  | "${" blank* (ident as id) blank* "}"   { Var id :: tokens lexbuf }
  | "\\$"                          { Text "$" :: tokens lexbuf }
  | "\\\\"                         { Text "\\" :: tokens lexbuf }
  | [^'$''\\']+ as s               { Text s :: tokens lexbuf }
  | _ as c                         { Text (String.make 1 c) :: tokens lexbuf }
  | eof                            { [] }

{
  open Printf

  let run function_name source ic oc =
    let lexbuf = Lexing.from_channel ic in
    let l = tokens lexbuf in
    let vars =
      let tbl = Hashtbl.create 10 in
      List.iter (
        function
          | Var id ->
              Hashtbl.replace tbl id ()
          | Text _ ->
              ()
      ) l;
      List.sort String.compare (Hashtbl.fold (fun k () acc -> k :: acc) tbl [])
    in
    fprintf oc "\
(* Auto-generated from %s. Do not edit. *)
let %s%s =

  String.concat \"\" [\n"
      source
      function_name
      (String.concat "" (List.map (fun s -> "\n  ~" ^ s) vars));
    List.iter (
      function
        | Var id ->
            fprintf oc "    %s;\n" id
        | Text s ->
            fprintf oc "    %S;\n" s
    ) l;
    fprintf oc "  ]\n"

  let main () =
    let out_file = ref None in
    let in_file = ref None in
    let function_name = ref "gen" in
    let options = [
      "-f",
      Arg.Set_string function_name,
      "<lowercase identifier>  Name of the OCaml function (default: gen)";

      "-o",
      Arg.String (
        fun s ->
          if !out_file <> None then
            failwith "Multiple output files"
          else
            out_file := Some s
      ),
      "<file>  Output file (default: output goes to stdout)";
    ]
    in
    let anon_fun s =
      if !in_file <> None then
        failwith "Multiple input files"
      else
        in_file := Some s
    in

    let usage_msg = sprintf "\
Usage: %s [input file] [options]

Convert a template into an OCaml function with labelled arguments.

  Hello ${x}

becomes:

  let gen ~x = \"Hello \" ^ x

Use a backslash character to escape $ or \\ itself (\\\\\\${x} gives \\${x}).

Options:
"
      Sys.argv.(0)
    in

    Arg.parse options anon_fun usage_msg;

    let ic, source =
      match !in_file with
          None -> stdin, "<stdin>"
        | Some file -> open_in file, file
    in
    let oc =
      match !out_file with
          None -> stdout
        | Some file -> open_out file
    in
    run !function_name source ic oc

  let () = main ()
}
