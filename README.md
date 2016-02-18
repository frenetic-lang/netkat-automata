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

The Felix compiler also depends on a couple of OCaml libraries that you can
install using `opam`:

```bash
opam install tdk hashcons
```

The Felix compiler can be run against topologies from the [Topology
Zoo][topo_zoo]. In order to do so, you have to install some specially encoded
policy files (`*.json`) and topology files (`*.dot`). To download and unpack
the files, run the following shell commands in the root of this github project:

```bash
git clone 'https://gist.github.com/a8839c2663d3aa30dc25.git' topozoo
cd topozoo
tar -xzvf topozoo.tgz
```

The `rlwrap` command line utility is quite useful when using the Felix compiler
as a REPL. You can install `rlwrap` on the frenetic VM with the following
command:

```bash
sudo apt-get install rlwrap
```

## Compiling Queries ##
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
$ rlwrap ./Compile.native inptout networks/line/{in,out,p,t}.kat
> (switch=1 and port=2, switch=2 and port=1)
Query
=====
(((switch = 1) ∧ (port = 2)), ((switch = 2) ∧ (port = 1)))

Compiled Terms
=============
(switch=1;port=1 + switch=2;port=2);(dst=1;port:=1 + dst=2;port:=2);switch=1;port=2;(switch=1;port=2;switch:=2;port:=1 + switch=2;port=1;switch:=1;port:=2);switch=2;port=1;(dst=1;port:=1 + dst=2;port:=2);(switch=1;port=1 + switch=2;port=2)

(alpha, beta) pairs
===================
([dst := 2;port := 1;switch := 1],[dst := 2;port := 2;switch := 2])

(alpha, beta) JSON
==================
{"switch":"1","port":"1","dst":"2"}
```

The compiler prints out the query, the compiled NetKAT term, the (α,β) pairs in
the E-matrix of the compiled term, and the JSON that is eventually shipped off
to the monitoring agents.

Alternatively, we can specify a query at the command line as follows:

```bash
$ make
$ echo "(switch=1 and port=2, switch=2 and port=1)" > query.txt
$ ./Compile.native inptout networks/line/{in,out,p,t}.kat query.txt
Query
=====
(((switch = 1) ∧ (port = 2)), ((switch = 2) ∧ (port = 1)))

Compiled Terms
=============
(switch=1;port=1 + switch=2;port=2);(dst=1;port:=1 + dst=2;port:=2);switch=1;port=2;(switch=1;port=2;switch:=2;port:=1 + switch=2;port=1;switch:=1;port:=2);switch=2;port=1;(dst=1;port:=1 + dst=2;port:=2);(switch=1;port=1 + switch=2;port=2)

(alpha, beta) pairs
===================
([dst := 2;port := 1;switch := 1],[dst := 2;port := 2;switch := 2])

(alpha, beta) JSON
==================
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
4 terms, 8 points:
  0.017881 ms of compilation
  18.470049 ms of generating alpha, beta pairs
> (pass, pass); (pass, pass)
4 terms, 4 points:
  0.027895 ms of compilation
  10.816813 ms of generating alpha, beta pairs
> (pass, pass); (pass, pass); (pass, pass)
4 terms, 0 points:
  0.063896 ms of compilation
  11.891842 ms of generating alpha, beta pairs
```

The REPL prints out how many intermediate terms the compiler used to compile
the query, how many (α,β) pairs were read from the E-matrix of the compiled
term, and how long various stages of the compiler took. The compiled term and
the (α,β) pairs are not printed because they can be quite large.

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

## Running Queries ##
In this section, we'll discuss how to compile a query into a set of predicates,
how to install the predicates on a set of end hosts running in a mininet
virtual network, and how to query the end hosts to get the packet counts. As a
running example, we'll measure the traffic over all 1-hop paths in the
Arpanet196912 topology.

First up, we need to compile a query into a set of predicates. The Felix
compiler encodes (α,β) pairs in JSON and outputs them into a file
`predicates.json`.

```bash
$ ./Compile.native zoo topozoo/Arpanet196912.{json,dot} queries/1edge.txt
Arpanet196912, 1edge, 3.211021, 8
$ cat predicates.json
[{"port":"1","switch":"1","ip4dst":"111.0.4.4/32"}, ..., {"port":"1","switch":"2","ip4dst":"111.0.1.1/32"}]
```

Next, we'll use the `JSONToPolicy` script to convert a topology zoo policy file
into NetKAT.
```bash
$ ./JSONToPolicy.native topozoo/Arpanet196912.json > Arpanet196912.kat
$ cat Arpanet196912.kat
filter ethTyp=0x800; (
filter switch = 3 and ip4Dst = 111.0.3.3; port := 1 |
filter switch = 1 and ip4Dst = 111.0.3.3; port := 3 |
...
filter switch = 1 and ip4Dst = 111.0.2.2; port := 2 |
filter switch = 4 and ip4Dst = 111.0.2.2; port := 2
)
```

Next, we'll use `TopoToMininet` to generate a mininet python script that will
launch our virtual network.

```bash
$ ./TopoToMininet.native mn_prologue.txt mn_epilogue.txt topozoo/Arpanet196912.dot > Arpanet196912.py
$ cat Arpanet196912.py
import re
import sys

# Mininet imports
from mininet.log import lg, info, error, debug, output
from mininet.util import quietRun
from mininet.node import Host, OVSSwitch, RemoteController
from mininet.cli import CLI
from mininet.net import Mininet
...
```

Instead of running these commands by hand, we can also use the
[`end2end.sh`](end2end.sh) script. Simply provide `end2end.sh` with a policy,
topology, and query, and it will produce all the files you need.

```bash
$ ./end2end.sh topozoo/Arpanet196912.{json,dot} queries/1edge.txt
Arpanet196912, 1edge, 3.819942, 8
Created predicates.json
Created Arpanet196912.kat
Created Arpanet196912.py
```

Next, move the `predicates.json`, `Arpanet196912.kat`, and `Arpanet196912.py`
over to a clone of the `measurement` repository. First, start the frenetic
shell and load the kat file.

```bash
$ rlwrap frenetic shell
Frenetic Shell v 4.0
Type `help` for a list of commands
frenetic> load Arpanet196912.kat
```

Then, in another window, run the mininet script. It's always a good idea to
make sure everything has been cleaned up properly before launching mininet.

```bash
sudo mn -c                          # cleanup mininet
sudo rlwrap python Arpanet196912.py # launch mininet
```

After the mininet topology loads, run the following command to configure the
monitors to count packets:

```bash
h1 python config_monitors -q predicates.json
```

Finally, run the `query_monitors.py` script to query the total packet counts.

```bash
h1 python query_monitors -q predicates.json
```

Sometimes, mininet doesn't kill things like it ought to. You can kill stuff
yourself:

```bash
sudo pkill -f "python ./agent.py"
sudo pkill -f "python ./monitor.py"
```

[mwhittaker_frenetic]: https://github.com/mwhittaker/frenetic
[netkat_json]:         https://github.com/frenetic-lang/frenetic/blob/master/lib/Frenetic_NetKAT_Json.ml
[topo_zoo]:            http://www.topology-zoo.org/
