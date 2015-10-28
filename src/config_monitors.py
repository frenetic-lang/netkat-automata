import requests, json, sys

def configure_monitors(host, configs, mappings):
    for config in configs:
        modified_configs = configsp_to_configip(config, mappings)
        # Adding all of the leaf agents to host
        added_leaf_agents = []
        for modified_config in modified_configs:    
            if modified_config['src'] == host or modified_config['src'] in added_leaf_agents:
                continue
            data = {}
            data['type'] = 'add_leaf_agent'
            data['agent_addr'] = modified_config['src']
            requests.post('http://' + host + ':8000', data=data)
            added_leaf_agents.append(data['agent_addr'])
        # Configure all of the monitors       
        for modified_config in modified_configs:    
            print modified_config
            requests.post('http://' + host + ':8000', data=modified_config)

def configure_counter(host, counter):
    counter['type'] = 'config_sketch_counter'
    counter['sketch_id'] = '0'
    print counter
    requests.post('http://' + host + ':8000', data=counter)
'''
config is dict of a single config file with possible switch and port field
mappings_dict is dict of tuple (switch, port) to ip

removes switch and port fields from config and replaces with ips.
one valid ip per config dict (rest of fields remain unchanged)
returns list containing all possible configs
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
    host = ''
    counter = {}
    configs = []
    mappings = {}
    for (i, arg) in enumerate(sys.argv):
        if arg == '--host' or arg == '-h':
            host = sys.argv[i + 1]
        if arg == '--config' or arg == '-c':
            with open(sys.argv[i + 1], 'r') as config_file:
                configs = json.loads(config_file.read())
        if arg == '--mappings' or arg == '-m':
            with open(sys.argv[i + 1], 'r') as mappings_file:
                mappings_lst = json.loads(mappings_file.read())
                for mapping in mappings_lst:
                    mappings[(mapping['switch'], mapping['port'])] = mapping['host']
        if arg == '--counter' or arg == '-n':
            with open(sys.argv[i + 1], 'r') as counter_file:
                counter = json.loads(counter_file.read())
    if len(host) == 0 or len(counter) == 0 or len(configs) == 0 or len(mappings) == 0:
        raise Exception ("Improperly specified input")
    configure_monitors(host, configs, mappings)
    configure_counter(host, counter)

main()
