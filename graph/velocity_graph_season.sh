#!/usr/bin/env -S bash -e
# GMT modern mode bash template
# Date:    2021-07-07T10:31:45
# User:    aorimbe
# Purpose: Purpose of this script
export GMT_SESSION_NAME=$$	# Set a unique session name
region="100/153.25/-13.41/30"
sv="v0.15i+e"
for i in 01 04 07 10
do
filename="/Users/aorimbe/Desktop/shuku/graph/adv_v_$i.dat"
cptin="/Users/aorimbe/Desktop/shuku/graph/adv_v_cpt_$i.dat"
cptout="/Users/aorimbe/Desktop/shuku/graph/adv_v_cpt_$i.nc"
gmt begin velocity_graph$i png

gmt xyz2grd $cptin -G$cptout  -I640+n/522+n -R$region
gmt makecpt -Cpanoply -T0.1/80
gmt grdimage -R$region $cptout -C -JM25i  
gmt plot $filename -R$region -JM25i -S$sv -W1p -Gblack
gmt coast -R$region -JM25i -W2p,black -Ggrey  -Bafg -BWSen+t"velocity_$i"
gmt end show
done