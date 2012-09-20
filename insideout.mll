{
type var = {
  ident : string;
  format : string option; (* "%d" *)
  default : string option;
}

type token =
  Var of var
| Text of string
}

let blank = [' ' '\t']
let space = [' ' '\t' '\r' '\n']
let ident = ['a'-'z']['a'-'z' '_' 'A'-'Z' '0'-'9']*
let graph = ['\033'-'\126']
let format_char = graph # ['\\' ':' '}']

rule tokens = parse
  | "${" space* (ident as ident) space*
                                   { let format = opt_format lexbuf in
                                     let default = opt_default lexbuf in
                                     Var { ident; format; default }
                                     :: tokens lexbuf
                                   }
  | "\\$"                          { Text "$" :: tokens lexbuf }
  | "\\\\"                         { Text "\\" :: tokens lexbuf }
  | [^'$''\\']+ as s               { Text s :: tokens lexbuf }
  | _ as c                         { Text (String.make 1 c) :: tokens lexbuf }
  | eof                            { [] }

and opt_format = parse
  | "%" format_char+ as format space*    { Some format }
  | ""                                   { None }

and opt_default = parse
  | ":"    { Some (string [] lexbuf) }
  | "}"    { None }

and string acc = parse
  | "}"                { String.concat "" (List.rev acc) }
  | "\\\\"             { string ("\\" :: acc) lexbuf }
  | "\\}"              { string ("}" :: acc) lexbuf }
  | "\\\n" blank*      { string acc lexbuf }
  | [^'}' '\\']+ as s  { string (s :: acc) lexbuf }
  | _ as c             { string (String.make 1 c :: acc) lexbuf }

{
  open Printf

  let error source msg =
    eprintf "Error in file %s: %s\n%!" source msg;
    exit 1

  let run function_name source ic oc =
    let lexbuf = Lexing.from_channel ic in
    let l = tokens lexbuf in
    let vars =
      let tbl = Hashtbl.create 10 in
      List.iter (
        function
          | Var x ->
              let id = x.ident in
              (try
                 let x0 = Hashtbl.find tbl id in
                 if x <> x0 then
                   error source (
                     sprintf
                       "Variable %s occurs multiple times with a \n\
                        different %%format or different default value."
                       id
                   )
                 else
                   Hashtbl.replace tbl id x
             with Not_found ->
                 Hashtbl.add tbl id x
            )
          | Text _ ->
              ()
      ) l;
      List.sort
        (fun a b -> String.compare a.ident b.ident)
        (Hashtbl.fold (fun k v acc -> v :: acc) tbl [])
    in
    let args =
      let l =
        List.map (
          function
            | { ident; default = Some default } ->
                sprintf "\n  ?(%s = %S)" ident default
            | { ident; default = None } ->
                "\n  ~" ^ ident
        ) vars
      in
      String.concat "" l
    in
    fprintf oc "\
(* Auto-generated from %s. Do not edit. *)
let %s%s () =

  String.concat \"\" [\n"
      source
      function_name
      args;
    List.iter (
      function
        | Var x ->
            let id = x.ident in
            (match x.format with
               | None ->
                   fprintf oc "    %s;\n" id
               | Some fmt ->
                   fprintf oc "    Printf.sprintf %S %s;\n" fmt id
            )
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

  let gen ~x () = \"Hello \" ^ x

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
