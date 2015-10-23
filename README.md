# Measurement #
This branch defines a path query language and a compiler from queries and
network topologies to NetKAT terms.

## Query Language ##
Our path query language can be understood as regular expressions over edges.
Edges are represented as pairs of predicates where the pair `(a, b)` matches
all edges where packets at the beginning of the edge match predicate `a` and
packets at the end of the edge match predicate `b`.

### Syntax ###

```
(* predicates *)
a, b ::= pass     (* 1     *)
       | drop     (* 0     *)
       | f = n    (* f = n *)
       | a or b   (* a ∨ b *)
       | a and b  (* a ∧ b *)
       | not a    (* ¬a    *)

(* queries *)
q, q' ::= (a, b) (* (a, b) *)
        | q + q' (* q + q' *)
        | q ; q' (* q • q' *)
        | q*     (* q*     *)
```
### Semantics ###
Let `R(p) = range([[p]])` where `p` is a NetKAT term and `[[•]]` is the
denotational semantics mapping NetKAT terms to functions from histories to sets
of histories. All query predicates are also NetKAT predicates, so we translate
between the two implicitly.

```
[[(a, b)]] = {pk_b::<pk_a> | pk_a ∈ R(a), pk_b ∈ R(b)}
[[q + q']] = [[q]] ∪ [[q']]
[[q ; q']] = {h'::h | h ∈ [[q]], h' ∈ [[q']]}
[[q*]]     = [[q^1]] ∪ [[q^2]] ∪ [[q^3]] ∪ ...
```

### Examples ###
Here are some example queries. We abbreviate `sw = p and pt = n` as `p:n`,
though this abbreviation is not part of the actual query language.

- `(a:2, b:1)` matches the single edge from switch `a` port 1 to switch `b` port `2`.
- `(drop, drop)` matches no edges, or equivalently matches all paths of length 0.
- `(pass, pass)` matches all edges, or equivalently matches all paths of length 1.
- `(pass, pass); (pass, pass)` matches all paths of length length 2.
- `(pass; pass)^n` matches all paths of length `n`.
- `(a:2, b:1); (b:2, c:1)` matches a single path of two edges.
- `(pass, pass)*; (a:2, b:1); (pass; pass)*` matches all paths that include the
  edge `(a:2, b:1)`.
- `(sw = a, sw = z) + ((sw = a, pass); (pass; pass)*; (pass, sw = z))` matches
  all paths from switch `a` to switch `z`.

## Compiler and REPL ##
The query language compiler logic is found in
[`src/Decide_Measurement.ml`](src/Decide_Measurement.ml).
[`src/Compile.ml`](src/Compile.ml) is an executable that acts as a compiler and
REPL. `Compile` takes five command line arguments. The first four (`in`, `out,
`p`, and `t`) are files which contain the NetKAT encodings of a network. The
last argument (`q`) is an optional file which contains a query. If a query is
provided, the compiler compiles the query into a NetKAT.

Here is an example of how to query the line topology found in
[`measurement_examples/line`](measurement_examples/line). Notice that `s` and
`p` are used for the switch and packet fields. Also, all switch names are
numbers, not letters.

```
$ make
$ ./Compile.native measurement_examples/line/{in,out,p ,t}.kat
> (s = 1 and p = 2, s = 2 and p = 1)
(((s = 1) ∧ (p = 2)), ((s = 2) ∧ (p = 1)))
(s=1;p=1 + s=2;p=2);(t=1;p:=1 + t=2;p:=2);s=1;p=2;(s=1;p=2;s:=2;p:=1 + s=2;p=1;s:=1;p:=2);s=2;p=1;(t=1;p:=1 + t=2;p:=2);(s=1;p=1 + s=2;p=2)

Points:
([t := 2;p := 1;s := 1],[t := 2;p := 2;s := 2])
```
