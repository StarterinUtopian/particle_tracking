#!/usr/bin/env -S bash -e
# GMT modern mode bash template
# Date:    2021-10-06T13:43:42
# User:    aorimbe
# Purpose: Purpose of this script

export GMT_SESSION_NAME=$$	# Set a unique session name
region="118/150/10/20"
filename="/Volumes/erika/CMEMS/output/count/source0320.dat"
gmt begin source png
# 
gmt coast -R$region -JM25i -W0.1p,black -Gblack -Sgrey -Bafg -BWSen
# gmt plot -R$region $filename -Sc0.1c -W0.1p -CIcpt.cpt
gmt makecpt -Crainbow -T1/14/1 
gmt plot -R$region  $filename -JM25i -Sc5p -C
gmt end show



