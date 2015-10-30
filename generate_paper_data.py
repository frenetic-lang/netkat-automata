import csv, os, math

queries = ['no_paths', '5edge', '1allstar', 'node4or5']
queries_benchmarks = {}
queries_sizes = {}
for query in queries:
    queries_benchmarks[query] = []
    queries_sizes[query] = []

topos = []
with open('benchWithSize.txt', 'rb') as bench_file:
    reader = csv.reader(bench_file, delimiter=",")
    for row in reader:
        topos.append([element.strip() for element in row])

for topo in topos:
    for query in queries:
        if query == topo[1]:
            queries_benchmarks[query].append(topo[2])
            queries_sizes[query].append(topo[4])

for query in queries:
    with open('./scatterplot_data/' + query + '.txt', 'w') as f:
        benchmarks = queries_benchmarks[query]
        sizes = queries_sizes[query]
        for i in xrange(0, len(benchmarks)):
            f.write(sizes[i] + ' ' + str(float(benchmarks[i]) / 1000) + '\n')

selected_topos = ['JanetExternal', 'Claranet', 'Quest', 'Integra', 'GtsPoland', 'Reuna', 'Internode', 'Arpanet196912', 'Oteglobe']

for selected_topo in selected_topos:
    with open('./barplot_data/' + selected_topo + '.txt', 'w') as f:
        for i, query in enumerate(queries):
            for topo in topos:
                if topo[0] == selected_topo and topo[1] == query:
                    f.write(str(i) + ' "' + query + '" ' + str(float(topo[2]) / 1000) + '\n')

for query in queries:
    os.popen('gnuplot -e "query=\'' + query + '\'" scatterplot.gnu') 

for selected_topo in selected_topos:
    os.popen('gnuplot -e "topo=\'' + selected_topo + '\'" barplot.gnu') 

