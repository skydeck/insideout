insideout
=========

Description
-----------

Convert a template into an OCaml function with labelled arguments.

This is a camlp4-free replacement of the "interpolate file" feature of
[xstrp4](http://projects.camlcity.org/projects/xstrp4.html).

```
Hello ${name}
```

becomes:

```ocaml
let gen ~name = "Hello " ^ name
```
Use a backslash character to escape $ or \ itself (\\\${x} gives \${x}).

Use `insideout -help` to see the available options.

Installation
------------

(requires a standard OCaml installation)

```
$ make
$ make install
```
