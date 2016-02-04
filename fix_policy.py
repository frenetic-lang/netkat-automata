import sys
import json

def fix_policy(pol):
  switch, ip = pol['pols'][0]['pred']['preds']
  switch['value'] += 1
  addr = ip['value']['addr'].split('.')
  addr[2] = str(int(addr[2]) + 1)
  addr[3] = str(int(addr[3]) + 1)
  ip['value']['addr'] = '.'.join(addr)

def fix_file(filename):
  with open(filename) as f:
    j = json.loads(f.read())

  for pol in j['pols']:
    fix_policy(pol)

  with open(filename, 'w') as f:
    f.write(json.dumps(j))

if __name__ == "__main__":
  for arg in sys.argv[1:]:
    print "Fixing file: %s" % arg
    fix_file(arg)
