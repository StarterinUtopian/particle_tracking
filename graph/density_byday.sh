#!/usr/bin/env -S bash -e
# GMT modern mode bash template
# Date:    2021-10-06T13:43:42
# User:    aorimbe
# Purpose: Purpose of this script



for i in "051" "101" "151" "201"

do

export GMT_SESSION_NAME=$$	# Set a unique session name
region="100/155/-15/30"
# infile="/Volumes/erika/CMEMS/output/density/${j}0${i}.dat"
# outfile="/Volumes/erika/CMEMS/output/density/${j}0${i}01.nc"
# infile="/Volumes/erika/CMEMS/output/density/byday/${i}.dat"
# outfile="/Volumes/erika/CMEMS/output/density/byday/${i}.nc"
# gmt xyz2grd $infile -G$outfile -I0.1/0.1 -R$region
gmt begin trajectory_colorbar png


gmt makecpt -Crainbow -T1/200
# gmt grdimage -R$region  $outfile   -C -JM25c
# gmt coast -R$region -JM25c -W0.1p,black -Ggrey -Bafg -BWSen
gmt colorbar -Dg100/0+w5/0.5 -Baf -R$region
gmt end show
done


