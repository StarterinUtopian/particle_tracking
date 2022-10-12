#!/usr/bin/env -S bash -e
# GMT modern mode bash template
# Date:    2021-07-09T08:30:18
# User:    aorimbe
# Purpose: Purpose of this script
# export GMT_SESSION_NAME=$$	# Set a unique session name
# region="100.00/153.25/-13.42/30.00"
# filename="/Volumes/CNMES2006-2019/data/2018/01/20180101.nc"
# gmt begin figurename
# gmt coast -R$region -JM8i -W0.1p,black -Gdarkseagreen2 -Swhite -Bafg -BWSen+t"temperature"
# gmt makecpt -Cpanoply -T19/23>tmp.cpt
# gmt colorbar -Dx0c/0c+w10c/1c+h -B0
# data=`gmt grdinfo "$filename?thetao(1,23)"|gmt grdcut -R$region` 
# gmt grdimage $data  -C
# gmt end show

# !/usr/bin/env -S bash -e
# GMT modern mode bash template
# Date:    2021-07-07T10:31:45
# User:    aorimbe
# Purpose: Purpose of this script

export GMT_SESSION_NAME=$$	# Set a unique session name
region="100/153.25/-13.42/30"

in_s="/Users/aorimbe/Desktop/shuku/adverage/adv_s.dat"
out_s="/Users/aorimbe/Desktop/shuku/adverage/adv_s.nc"
gmt begin salinity png

gmt xyz2grd $in_t -G$out_s  -I640+n/522+n -R$region
gmt makecpt -Cpanoply -T30/40
gmt grdimage -R$region $out_s -C -JM10c  
gmt coast -R$region -JM10c -W1p,black -Gwhite  -Bafg -BWSen
gmt end show

