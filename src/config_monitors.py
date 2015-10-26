import requests, json, sys

def configure_monitors(hosts, configs):
    for host in hosts:
        for config in configs:
            requests.post(host, config)

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

