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

Printf formatting:
```
${x %f}
```
(formats the argument `x` using `Printf.sprintf "%f" x`)

File inclusion (as-is, no escaping, no substitutions):
```
${@foo/bar}
```
(includes the contents of file `foo/bar`)

Default values (useful for previews or suggested usage):
```
${title:Here goes the title}
```

Use `insideout -help` to see the command-line options.


Installation
------------

(requires a standard OCaml installation)

```
$ make
$ make install
```

Example
-------

See file `example.html` and run demo with:
```
$ make demo
```
