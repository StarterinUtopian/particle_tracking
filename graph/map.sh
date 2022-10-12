#!/usr/bin/env -S bash -e
# GMT modern mode bash template
# Date:    2021-10-06T13:43:42
# User:    aorimbe
# Purpose: Purpose of this script

export GMT_SESSION_NAME=$$	# Set a unique session name
region="110/150/-15/31"
gmt begin map png
gmt coast -R$region -JM25c  -Gblack  
gmt end show

