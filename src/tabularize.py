"""
Script to generate table for the evaluation section of the measurement paper.
The table has the following format:

    Topology | Term Size | # Switches | Query1 | Query2 | Query3 | ...
"""

import csv
import sys

# If true, this script creates a super big table. Otherwise, it creates a table
# that would fit in the paper.
BIG_TABLE = False

def usage():
    return 3, "usage: python tabularize.py <term_size> <num_switches> <bench>"

def setlist(iterator):
    return sorted(set(x for x in iterator))

def topos(bench):
    if BIG_TABLE:
        return setlist(k[0] for k in bench)
    else:
        return [
            "Goodnet",
            "Karen",
            "Canerie",
            "Intranetwork",
            "Internode",
            "Columbus",
            "Oteglobe",
        ]


def queries(bench):
    if BIG_TABLE:
        # return setlist(k[1] for k in bench)
        return [
            "no_paths",
            "1edge",
            "2edge",
            "3edge",
            "4edge",
            "5edge",
            "lessThan5edge",
            "1allstar",
            "2allstar",
            "3allstar",
            "4allstar",
            "5allstar",
            "nestedstar",
            "edge45",
            "edge23or45",
            "edge23or45inner",
            "node5",
            "node4or5",
            "node4or5inner",
            "path4536",
            "path4567",
            "path_123_456_789_101112",
            "1to10",
        ]
    else:
        return [
            "no_paths",
            "1edge",
            "5edge",
            "1allstar",
            "5allstar",
            "nestedstar",
            "node4or5",
            "path_123_456_789_101112",
        ]

def prune_bench(bench):
    good_topos = {t for t in topos(bench)
                    if all((t, q) in bench for q in queries(bench))}
    return {k: v for (k, v) in bench.iteritems()
                 if k[0] in good_topos}

def translate_query(query):
    return query

def format_bench(time, num_terms):
    return "{:.2f} / {}".format(time, num_terms)
    # return "{}".format(time)

def parse_term_size(filename):
    with open(filename, "r") as f:
        """ {topo: term size} """
        reader = csv.reader(f)
        return {row[0].strip(): int(row[1]) for row in reader}

def parse_num_switches(filename):
    """ {topo: num_switches} """
    with open(filename, "r") as f:
        reader = csv.reader(f)
        return {row[0].strip(): int(row[1]) for row in reader}

def parse_bench_file(filename):
    """ {(topo, query): (compile time, number of results) """
    with open(filename, "r") as f:
        reader = csv.reader(f)
        return {(row[0].strip(), translate_query(row[1].strip())):
                    (float(row[2]), int(row[3]))
                    for row in reader}

def make_table(term_size, num_switches, bench):
    header = ["Topology", "Term Size", "# Switches"] + queries(bench)
    body = [
        [t, term_size[t], num_switches[t]] +
        [format_bench(bench[t, q][0], bench[t, q][1]) for q in queries(bench)]
        for t in topos(bench)
    ]
    return [header] + body

def main(args):
    if len(args) != 3:
        print usage()
        sys.exit(-1)

    term_size_file = args[0]
    num_switches_file = args[1]
    bench_file = args[2]

    term_size = parse_term_size(term_size_file)
    num_switches = parse_num_switches(num_switches_file)
    bench = prune_bench(parse_bench_file(bench_file))

    writer = csv.writer(sys.stdout)
    for row in make_table(term_size, num_switches, bench):
        writer.writerow(row)

if __name__ == "__main__":
    main(sys.argv[1:])
