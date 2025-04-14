#!/usr/bin/gnuplot
reset

set term x11 1 font 20
set xlabel "Years" ; set ylabel "Benthic O2 (umol/kg)"
set grid
plot "../../cgenie_output/EXP1/biogem/biogem_series_ocn_O2.res" u 1:($5*(1E+6)) title "EXP1.SPIN" w l lw 2;
replot "../../cgenie_output/EXP2/biogem/biogem_series_ocn_O2.res" u 1:($5*(1E+6)) title "EXP2.SPIN" w l lw 3;

set terminal postscript color font 20
set output "|ps2pdf - Benthic_O2.pdf"
replot

