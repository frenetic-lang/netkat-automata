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
