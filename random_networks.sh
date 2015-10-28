#! /bin/bash

set -euo pipefail

main() {
    if [[ $# < 2 ]]; then
        echo "usage: random_networks.sh <topos> <n>"
        exit -1
    fi

    topos="$1"
    n="$2"
    shuf "$1" | head -n "$n" | sort -n | awk '{print $2}'
}

main "$@"
