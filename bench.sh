#! /bin/bash

set -euo pipefail

main() {
    for topofile in "$@"; do
        topo="$(basename $topofile .json)"
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
    done
}

main "$@"
