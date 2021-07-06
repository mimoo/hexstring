# Hexstring

A safe-to-use OCaml library to encode/decode hexadecimal strings.

Install with opam:

```console
opam install hexstring
```

Add to your dune file:

```lisp
(libraries hexstring)
```

Use as such:

```ocaml
(* you can encode bytes to an hexstring *)
b = Bytes.of_string "\x01\x02";;
s = Hexstring.encode b;; (* "0102" *)

(* you can also decode some hexstring, which returns a result of bytes *)
d = match Hexstring.decode s with
| Error err -> printf "error: %s\n"
| Ok b' -> assert b = b'
```

Since it returns a `Result` type, it's on the caller to decide what to do with an invalid input. No functions will panic on you.

Oh, and it's fast:

```
┌──────────┬──────────┬─────────┬────────────┐
│ Name     │ Time/Run │ mWd/Run │ Percentage │
├──────────┼──────────┼─────────┼────────────┤
│ encoding │   1.17us │  54.00w │     99.35% │
│ decoding │   1.18us │ 216.00w │    100.00% │
└──────────┴──────────┴─────────┴────────────┘
```
