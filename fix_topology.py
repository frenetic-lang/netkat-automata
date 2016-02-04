"""
Due to a (mininet?) bug, topology files cannot have switches numbered 0. This
script goes through line by line and (hackily) increments all switch ids, IP
addresses, and MAC addresses. It also fixes a bug where MAC addresses weren't
being expressed in hex.
"""

import re
import sys

def main(filenames):
    if len(filenames) != 1:
        print "usage: python fix_topology.py <topo.dot>"
        sys.exit(-1)

    with open(filenames[0], "r") as f:
        for line in f:
            if "id=" in line:
                r = "(.*id=)([0-9]+)(,.*)"
                m = re.search(r, line)
                new_id = str(int(m.group(2)) + 1)
                print re.sub(r, r"\g<1>" + new_id + r"\g<3>", line),
            elif "mac=" in line:
                r = '(.*mac=")(.*)(",.*)'
                m = re.search(r, line)
                mac = [int(x) + 1 for x in m.group(2).split(":")]
                new_mac = ":".join("%02x" % x for x in mac)
                print re.sub(r, r"\g<1>" + new_mac + r"\g<3>", line),
            elif "ip=" in line:
                r = '(.*ip=")(.*)(",.*)'
                m = re.search(r, line)
                ip = [int(x) for x in m.group(2).split(".")]
                ip = [ip[0], ip[1], ip[2] + 1, ip[3] + 1]
                new_ip = ".".join(str(x) for x in ip)
                print re.sub(r, r"\g<1>" + new_ip + r"\g<3>", line),
            else:
                print line,

if __name__ == "__main__":
    main(sys.argv[1:])
