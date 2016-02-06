set terminal post "Helvetica" 15
set output "| ps2pdf - ./plots/combined.pdf"

one = "#99ffff"; two = "#4671d5"; three = "#ff0000"; four = "#f36e00"; five = "#33CC33"; six = "#CC33FF"; seven = "#663300"

set key left
set ylabel "Time (s)"
set auto x
set logscale y
set style data histogram
set style histogram cluster gap 1
set style fill solid border -1
set boxwidth 0.9
set xtic scale 0
plot './barplot_data/combined.txt' using 2:xtic(1) ti col fc rgb one, '' u 3 ti col fc rgb two, '' u 4 ti col fc rgb two, '' u 5 ti col fc rgb two, '' u 6 ti col fc rgb two, '' u 7 ti col fc rgb two, '' u 8 ti col fc rgb three, '' u 9 ti col fc rgb three, '' u 10 ti col fc rgb three, '' u 11 ti col fc rgb three, '' u 12 ti col fc rgb three, '' u 13 ti col fc rgb seven, '' u 14 ti col fc rgb five 
