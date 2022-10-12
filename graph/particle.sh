#!/usr/bin/env -S bash -e
# GMT modern mode bash template
# Date:    2021-10-06T13:43:42
# User:    aorimbe
# Purpose: Purpose of this script
# for((j=2008;j<2019;j++))
j=2006
# {
for ((i=1;i<10;i++))
{
export GMT_SESSION_NAME=$$	# Set a unique session name
region="120/210/-60/20"
filename="/Volumes/south_pacific/output/all/${j}0${i}01.dat"
gmt begin ${j}0${i}01 png
# 
gmt coast -Rg$region -JX25c -W0.1p,black -Gblack -Sgrey -Bafg -BWSen+t"${j}0${i}01"
# gmt plot -R$region $filename -Sc0.1c -W0.1p -CIcpt.cpt
gmt makecpt -Crainbow -T0/201/1 
gmt plot -R$region  $filename -JX25c -Sc0.5p -C -Q
gmt end show
}
for ((i=10;i<13;i++))
{
export GMT_SESSION_NAME=$$	# Set a unique session name
region="120/210/-60/20"
filename="/Volumes/south_pacific/output/all/${j}${i}01.dat"
gmt begin ${j}${i}01 png
# gmt grdimage @earth_relief_01m -JM25i -R$region -Cgeo 
gmt coast -Rg$region -JX25c -W0.1p,black -Gblack -Sgrey -Bafg -BWSen+t"${j}${i}01"
# gmt plot -R$region $filename -Sc0.1c -W0.1p -CIcpt.cpt
gmt makecpt -Crainbow -T0/201/1 
gmt plot -Rg$region  $filename -JX25c -Sc0.5p -C
# echo 40p 1 | gmt text -F+f 40p 1 -pdf 2017${i}01
gmt end show
}

}
