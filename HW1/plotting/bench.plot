set term png
set output "results.png"
set title "Requests handled per time unit"
set xlabel "Requests"
set ylabel "Time"
set xrange [0:1000]
set yrange [0:10000000]

plot "withDelay.dat" using 1:2 with linespoints title "with delay (µs)",\
    "noDelay.dat" using 1:2 with linespoints title "NO delay (ms)"
    
