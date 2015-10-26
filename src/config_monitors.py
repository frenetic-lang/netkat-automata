import requests, json, sys

def configure_monitors(hosts, configs):
    for host in hosts:
        for config in configs:
            requests.post(host, config)
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
      for k, v in mappings_dict:
        if(k == (config['switch'], config['port'])):
            config.pop('switch')
            config.pop('port')
            config['IP'] = v
            new_configs_lst.append(config)
            return new_configs_lst
       raise Exception("Mapping not found for port " + str(config['port']) + ' switch ' + str(config['switch']))
  elif("port" in config):
      for k, v in mappings_dict:
          if(k[1] == config['port']):
             n_config = config.copy()
             n_config.pop('port')
             n_config['IP'] = v
             new_configs_lst.append(n_config)
       if(len(new_configs_lst) == 0):
          raise Exception("No mappings found for port: " + str(config['port']))
       return new_configs_lst
   elif("switch" in config):
      for k, v in mappings_dict:
          if(k[0] == config['switch']):
              n_config = config.copy()
              n_config.pop('switch')
              n_config['IP'] = v
              new_configs_lst.append(n_config)
      if(len(new_configs_lst) == 0):
          raise Exception("No mappings found for switch: " + str(config['switch']))
    else:
        new_configs_lst.append(config)
        return new_configs_lst
          

def main():
    hosts = []
    configs = []
    for (i, arg) in sys.argv:
        if arg == "--hosts" or "-h":
            with open(sys.argv[i + 1], 'r') as hosts_file:
                for host in hosts_file:
                    hosts.append(host)
        if arg == "--config" or "-c":
            with open(sys.argv[i + 1], 'r') as config_file:
                configs = json.loads(config_file.read())
    configure_monitors(hosts, configs)

