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

  let escape s =
    let buf = Buffer.create (2 * String.length s) in
    for i = 0 to String.length s - 1 do
      match s.[i] with
        | '$' -> Buffer.add_string buf "\\$"
        | '\\' -> Buffer.add_string buf "\\\\"
        | c -> Buffer.add_char buf c
    done;
    Buffer.contents buf

  let error source msg =
    eprintf "Error in file %s: %s\n%!" source msg;
    exit 1

  let parse_template source ic oc =
    let lexbuf = Lexing.from_channel ic in
    let l = tokens lexbuf in
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
    tbl, l

  let emit_ocaml use_defaults function_name source oc var_tbl l =
    let vars =
      List.sort
        (fun a b -> String.compare a.ident b.ident)
        (Hashtbl.fold (fun k v acc -> v :: acc) var_tbl [])
    in

    fprintf oc "(* Auto-generated from %s. Do not edit. *)\n" source;
    List.iter (
      fun x ->
        match x.default with
          None -> ()
        | Some s -> fprintf oc "let default_%s = %S\n" x.ident s
    ) vars;

    let args =
      let l =
        List.map (
          function
            | { ident; default = Some default } when use_defaults ->
                sprintf "\n  ?(%s = default_%s)" ident ident
            | { ident } ->
                "\n  ~" ^ ident
        ) vars
      in
      String.concat "" l
    in
    fprintf oc "\
let %s%s () =

  String.concat \"\" [\n"
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

  let expand_defaults esc oc l =
    List.iter (
      function
        | Var x ->
          let id = x.ident in
          (match x.default with
              None ->
                let opt_format =
                  match x.format with
                      None -> ""
                    | Some s -> " " ^ s
                in
                fprintf oc "${%s%s}" id opt_format
            | Some s ->
                output_string oc (if esc then escape s else s)
          )
        | Text s ->
            output_string oc (if esc then escape s else s)
    ) l

  let ocaml use_defaults function_name source ic oc =
    let var_tbl, l = parse_template source ic oc in
    emit_ocaml use_defaults function_name source oc var_tbl l

  let preview esc function_name source ic oc =
    let var_tbl, l = parse_template source ic oc in
    expand_defaults esc oc l

  let main () =
    let out_file = ref None in
    let in_file = ref None in
    let function_name = ref "gen" in
    let use_defaults = ref true in
    let mode = ref `Ocaml in
    let options = [
      "-f",
      Arg.Set_string function_name,
      "<lowercase identifier>
          Name of the OCaml function (default: gen)";

      "-o",
      Arg.String (
        fun s ->
          if !out_file <> None then
            failwith "Multiple output files"
          else
            out_file := Some s
      ),
      "<file>
          Output file (default: output goes to stdout)";

      "-no-defaults",
      Arg.Clear use_defaults,
      "
          Produce an OCaml function with only required arguments, ignoring
          the defaults found in the template.";

      "-preview",
      Arg.Unit (fun () -> mode := `Preview),
      "
          Preview mode: substitute variables which have a default value,
          leave others intact.";

      "-xdefaults",
      Arg.Unit (fun () -> mode := `Xdefaults),
      "
          Expand the defaults like in -preview mode but produce a valid
          template, keeping special characters escaped.";
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

Convert a template into an OCaml function with labeled arguments.

  Hello ${x}

becomes:

  let gen ~x () = \"Hello \" ^ x

Use a backslash character to escape $ or \\ itself (\\\\\\${x} gives \\${x}).

Also supported are %% format strings (as supported by OCaml's Printf):

  You are user number ${num %%i}.

Finally, default values can be specified after a colon:

  <title>${title:Welcome} to our user number ${ num %%i :1234}!</title>

Command-line options:
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
    match !mode with
        `Ocaml -> ocaml !use_defaults !function_name source ic oc
      | `Preview -> preview false !function_name source ic oc
      | `Xdefaults -> preview true !function_name source ic oc

  let () = main ()
}
