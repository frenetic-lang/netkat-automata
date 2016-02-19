#! /bin/bash

set -euo pipefail

f() {
    topo="$(basename $1 .json)"
    date
    echo $topo start
    ./Compile.native zoo topozoo/"$topo".{json,dot} \
        queries/no_paths.txt \
        queries/1edge.txt \
        queries/5edge.txt \
        queries/1allstar.txt \
        queries/5allstar.txt \
        queries/node4or5.txt \
        queries/path_123_456_789_101112.txt \
        queries/port80.txt \
        > "bench/$topo.txt"
    date
    echo "$topo done"
}

main() {
    export -f f
    parallel f ::: "$@"
}

main "$@"
