set terminal post "Helvetica" 30

set output "| ps2pdf - ./plots/".query.".pdf"

set multiplot
set key off
set ylabel "Time to solve (s)"
set xlabel "Total Term size"

set boxwidth 1
set style fill solid

set size ratio .6666

set logscale xy

#set yrange [min:max]

#set xrange [1000:132000]

plot "./scatterplot_data/".query.".txt" using 1:2 with points pointtype 7 pointsize 1.3 lt rgb "blue"

