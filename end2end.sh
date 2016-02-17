#! /bin/bash

set -euo pipefail

usage() {
    echo "end2end.sh policy.json topology.dot query.txt"
}

check_file_exists() {
    if [[ ! -f "$1" ]]; then
        echo "ERROR: $1 does not exist."
        exit -1
    fi
}

main() {
    if [[ "$#" -ne 3 ]]; then
        usage
        exit -1
    fi

    check_file_exists Compile.native
    check_file_exists JSONToPolicy.native
    check_file_exists TopoToMininet.native

    readonly p="$1"
    readonly t="$2"
    readonly q="$3"
    readonly name="$(basename -s .json $p)"

    ./Compile.native zoo "$p" "$t" "$q"
    echo "Created predicates.json"
    ./JSONToPolicy.native "$p" > "$name.kat"
    echo "Created $name.kat"
    ./TopoToMininet.native mn_prologue.txt mn_epilogue.txt "$t" > "$name.py"
    echo "Created $name.py"
}

main "$@"
