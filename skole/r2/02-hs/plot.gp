set title 'Fibonacci'
set grid

set terminal pngcairo enhanced size 800,600
set output "fibonacci.png"

plot 'fibonacci.dat' using 1:2 with linespoints
