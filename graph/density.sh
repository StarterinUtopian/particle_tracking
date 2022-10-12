#!/usr/bin/env -S bash -e
# GMT modern mode bash template
# Date:    2021-10-06T13:43:42
# User:    aorimbe
# Purpose: Purpose of this script



# for((j=2006;j<2019;j++))
# for((j=2006;j<2007;j++))
# {
# for i in `seq 1 1 9`
# do

# export GMT_SESSION_NAME=$$	# Set a unique session name
region="100/155/-15/30"
# region="100/153.25/-13.41/30"
# infile="/Volumes/erika/CMEMS/output/density/${j}0${i}01.dat"
# outfile="/Volumes/erika/CMEMS/output/density/${j}0${i}01.nc"
# infile="/Volumes/erika/CMEMS/output/density/month_adverage/month_adverage0${i}.dat"
# outfile="/Volumes/erika/CMEMS/output/density/month_adverage/month_adverage0${i}_new.nc"
# gmt xyz2grd $infile -G$outfile -I0.1/0.1 -R$region
# gmt xyz2grd $infile -G$outfile -I640+n/522+n -R$region
# gmt begin reach_density${j}${i}_new png
gmt begin reach_density${j}${i}_new png
gmt begin reach_density${j}${i}_new png
gmt basemap 
# gmt grdimage @earth_relief_01m -JM25i -R$region -Cgeo 
# gmt plot -R$region $filename -Sc0.1c -W0.1p -CIcpt.cpt

gmt makecpt -Cpanoply -T1/100
# gmt grdimage -R$region  $outfile   -C -JM25c
# gmt grdimage -R$region  $outfile   -C -JM25c
# gmt coast -R$region -JM25c -W0.1p,black -Gwhite  -Bafg -BWSen+t"adv_0${i}"
gmt colorbar -Dg100/0+w5/0.5 -Baf -R$region
gmt end show
# done
# for ((i=10;i<13;i++))
# {
# export GMT_SESSION_NAME=$$	# Set a unique session name
# region="100/155/-15/30"
# # infile="/Volumes/erika/CMEMS/output/density/${j}${i}01.dat"
# # outfile="/Volumes/erika/CMEMS/output/density/${j}${i}01.nc"
# # infile="/Volumes/erika/CMEMS/output/density/${j}0${i}01.dat"
# # outfile="/Volumes/erika/CMEMS/output/density/${j}0${i}01.nc"
# infile="/Volumes/erika/CMEMS/output/density/month_adverage/month_adverage${i}.dat"
# outfile="/Volumes/erika/CMEMS/output/density/month_adverage/month_adverage${i}.nc"
# gmt xyz2grd $infile -G$outfile -I0.1/0.1 -R$region
# gmt begin adv_${i}_new png
# # gmt grdimage @earth_relief_01m -JM25i -R$region -Cgeo 

# # gmt plot -R$region $filename -Sc0.1c -W0.1p -CIcpt.cpt
# # gmt makecpt -Chaxby -T1/130>tt.cpt
# gmt grdimage -R$region  $outfile  -C>tt.cpt -JM25c
# gmt coast -R$region -JM25c -W0.1p,black -Gblack  -Bafg -BWSen+t"adv_${i}"
# # echo 40p 1 | gmt text -F+f 40p 1 -pdf 2017${i}01
# gmt end show
# }

# }
