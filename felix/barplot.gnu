set terminal post "Helvetica" 15
set output "| ps2pdf - ./plots/".topo.".pdf"

set key off
set ylabel "Time to solve (s)"
set xtics rotate
set xlabel "Query"

set boxwidth 0.5
set style fill solid
plot "./barplot_data/".topo.".txt" using 1:3:xtic(2) with boxes
