# Felix #
This branch implements the Felix query language compiler. This README discusses
how to get the compiler up and running; it doesn't describe Felix at all. For
more information on Felix, please see our SOSR 2016 paper.

## Dependencies ##
The Felix compiler depends on a forked implementation of [some NetKAT JSON
parsing code][netkat_json] which can be found in the [mwhittaker fork of the
frenetic repository][mwhittaker_frenetic]. Execute the following shell commands
to clone and install the fork:

```bash
cd $HOME
git clone https://github.com/mwhittaker/frenetic.git --branch felix_json
opam pin add frenetic frenetic -k git
```

The Felix compiler can be run against topologies from the [Topology
Zoo][topo_zoo]. In order to do so, you have to install some specially encoded
policy files (`*.json`) and topology files (`*.dot`). To download and unpack
the files, run the following shell commands in the root of this github project:

```bash
wget http://www.cs.cornell.edu/~jnfoster/tmp/topozoo.tar.gz
tar -xzvf topozoo.tar.gz
```

The `rlwrap` command line utility is quite useful when using the Felix compiler
as a REPL. You can install `rlwrap` on the frenetic VM with the following
command:

```bash
sudo apt-get install rlwrap
```

## Getting Started ##
The query language compiler logic is found in
[`src/Decide_Measurement.ml`](src/Decide_Measurement.ml) (and
[`src/Decide_Measurement.mli`](src/Decide_Measurement.mli)).
[`src/Compile.ml`](src/Compile.ml) is the executable that acts as the compiler
and REPL. To build the compiler, simply run `make`.

The compiler has two subcommands, `inptout` and `zoo`, invoked with
`./Compile.native inptout` and `./Compile.native zoo` respectively. The first
subcommand takes five command line arguments. The first four (`in`, `out`, `p`,
and `t`) are files which contain the NetKAT encodings of a network. The last
argument (`q`) is an optional file which contains a query. If a query is
provided, the compiler compiles the query into a NetKAT term. Otherwise, the
compiler acts as a REPL reading queries from the command line.

Here is an example of how to query the line topology found in
[`measurement_examples/line`](measurement_examples/line) via the REPL. Note
that all switch names are numbers, not letters.

```bash
$ make
$ rlwrap ./Compile.native inptout measurement_examples/line/{in,out,p,t}.kat
> (switch=1 and port=2, switch=2 and port=1)
(((switch = 1) ∧ (port = 2)), ((switch = 2) ∧ (port = 1)))
(switch=1;port=1 + switch=2;port=2);(dst=1;port:=1 + dst=2;port:=2);switch=1;port=2;(switch=1;port=2;switch:=2;port:=1 + switch=2;port=1;switch:=1;port:=2);switch=2;port=1;(dst=1;port:=1 + dst=2;port:=2);(switch=1;port=1 + switch=2;port=2)

Points:
([dst := 2;port := 1;switch := 1],[dst := 2;port := 2;switch := 2])

JSON Packets for Measurement:
{"switch":"1","port":"1","dst":"2"}
```

The compiler prints out the query, the compiled NetKAT term, the (α,β) pairs in
the E-matrix of the compiled term, and the JSON that is eventually shipped off
to the monitoring agents.

Alternatively, we can specify a query at the command line as follows:

```bash
$ make
$ ./Compile.native inptout measurement_examples/line/{{in,out,p,t}.kat,q.query}
> (switch=1 and port=2, switch=2 and port=1)
(((switch = 1) ∧ (port = 2)), ((switch = 2) ∧ (port = 1)))
(switch=1;port=1 + switch=2;port=2);(dst=1;port:=1 + dst=2;port:=2);switch=1;port=2;(switch=1;port=2;switch:=2;port:=1 + switch=2;port=1;switch:=1;port:=2);switch=2;port=1;(dst=1;port:=1 + dst=2;port:=2);(switch=1;port=1 + switch=2;port=2)

Points:
([dst := 2;port := 1;switch := 1],[dst := 2;port := 2;switch := 2])

JSON Packets for Measurement:
{"switch":"1","port":"1","dst":"2"}
```

The second subcommand takes two mandatory command line arguments, `policy` and
`topology`: a JSON encoded policy and DOT encoded topology respectively. It
also takes any number of of optional query files that behave the same as with
the `inptout` subcommand; that is, if no queries are provided, then the
compiler acts as a REPL.

Here is an example of how to interactively query the 1969 Arpanet network in
the Topology Zoo.

```bash
$ rlwrap ./Compile.native zoo topozoo/Arpanet196912.{json,dot}
> (pass, pass)
8 points
> (pass, pass); (pass, pass)
4 points
> (pass, pass); (pass, pass); (pass, pass)
0 points
```

The REPL prints out how many (α,β) pairs were read from the E-matrix of the
compiled term. The compiled term itself is not printed because it can be quite
large.

Alternatively, we can query the topology non-interactively.

```bash
$ ./Compile.native zoo topozoo/Arpanet196912.{json,dot} queries/{1,2,3}edge.txt
Arpanet196912, 1edge, 3.211021, 8
Arpanet196912, 2edge, 2.306938, 4
Arpanet196912, 3edge, 3.557920, 0
```

The compiler prints out the name of the topology, the name of the query, the
time in ms to complete the query, and the number of (α,β) pairs that were read
from the E-matrix of the compiled term.

[mwhittaker_frenetic]: https://github.com/mwhittaker/frenetic
[netkat_json]:         https://github.com/frenetic-lang/frenetic/blob/master/lib/Frenetic_NetKAT_Json.ml
[topo_zoo]:            http://www.topology-zoo.org/
