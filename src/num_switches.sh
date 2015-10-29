#! /bin/bash

# Counts the number of switches in a set of topology DOT files.

set -eou pipefail

main() {
    for f in "$@"; do
        ns=$(grep "id=" "$f" | wc -l)
        filename=$(basename "$f")
        printf "${filename%.*}, $ns\n"
    done
}

main "$@"
