#!/usr/bin/env -S bash -e
# GMT modern mode bash template
# Date:    2021-10-06T13:43:42
# User:    aorimbe
# Purpose: Purpose of this script
# for((j=2008;j<2019;j++))


export GMT_SESSION_NAME=$$	# Set a unique session name
region="100/150/-15/30"
# filename="/Volumes/south_pacific/output/all/${j}${i}01.dat"
# filename="/Volumes/erika/CMEMS/processed_data/S/S_checkerr.dat"
# filename="/Volumes/erika/CMEMS/set_input/sealine.dat"
filename="/Volumes/erika/CMEMS/set_input/seashore.dat"
gmt begin coastline png
# gmt grdimage @earth_relief_01m -JM25i -R$region -Cgeo 
gmt coast -Rg$region -JX25c -W0.1p,black  -Sgrey -Bafg -BWSen+t"coastline"
# gmt plot -R$region $filename -Sc0.1c -W0.1p -CIcpt.cpt
# gmt makecpt -Crainbow -T0/201/1 
gmt makecpt -Cred
gmt plot -Rg$region  $filename -JX25c -Sc0.2p  -Gred
# echo 40p 1 | gmt text -F+f 40p 1 -pdf 2017${i}01
gmt end show

