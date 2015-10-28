import requests, json, sys

def configure_monitors(hosts, configs, mappings):
    for host in hosts:
        for config in configs:
            for modified_config in configsp_to_configip(config, mappings):
                print modified_config
                requests.post("http://" + host + ":8000", modified_config)
'''
config is dict of a single config file with possible switch and port field
mappings_dict is dict of tuple (switch, port) to IP

Removes switch and port fields from config and replaces with IPs.
One valid IP per config dict (rest of fields remain unchanged)
Returns list containing all possible configs
'''
def configsp_to_configip(config, mappings_dict):
  new_configs_lst = []
  if ("port" in config and "switch" in config):
      for k, v in mappings_dict.iteritems():
        if(k == (config['switch'], config['port'])):
            config.pop('switch')
            config.pop('port')
            config['src'] = v
            new_configs_lst.append(config)
            return new_configs_lst
        raise Exception("Mapping not found for port " + str(config['port']) + ' switch ' + str(config['switch']))
  elif("port" in config):
      for k, v in mappings_dict.iteritems():
          if(k[1] == config['port']):
             n_config = config.copy()
             n_config.pop('port')
             n_config['src'] = v
             new_configs_lst.append(n_config)
      if(len(new_configs_lst) == 0):
          raise Exception("No mappings found for port: " + str(config['port']))
      return new_configs_lst
  elif("switch" in config):
      for k, v in mappings_dict.iteritems():
          if(k[0] == config['switch']):
              n_config = config.copy()
              n_config.pop('switch')
              n_config['src'] = v
              new_configs_lst.append(n_config)
      if(len(new_configs_lst) == 0):
          raise Exception("No mappings found for switch: " + str(config['switch']))
      return new_configs_lst
  else:
        new_configs_lst.append(config)
        return new_configs_lst
          

def main():
    hosts = []
    configs = []
    mappings = {}
    for (i, arg) in enumerate(sys.argv):
        if arg == '--hosts' or arg == '-h':
            with open(sys.argv[i + 1], 'r') as hosts_file:
                for host in hosts_file.read().splitlines():
                    print host
                    hosts.append(host)
        if arg == '--config' or arg == '-c':
            with open(sys.argv[i + 1], 'r') as config_file:
                configs = json.loads(config_file.read())
        if arg == '--mappings' or arg == '-m':
            with open(sys.argv[i + 1], 'r') as mappings_file:
                mappings_lst = json.loads(mappings_file.read())
                for mapping in mappings_lst:
                    mappings[(mapping['switch'], mapping['port'])] = mapping['host']
    configure_monitors(hosts, configs, mappings)
main()
