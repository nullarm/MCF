#!/usr/bin/awk -f
# Template engine. 
# Usage: ./vars.awk vars.mcf io_config.mcf > io_new.mcf
# Replaces variables in io_config.mcf with pairs found in vars.mcf


BEGIN {
    OFS=FS="="
}

NR==FNR && NF>1 {
    # read a template file
    varname = $1
    gsub(/ /, "", varname)
    var[varname]=$2
    next
}

# skip comments
$0~/#/{
    print
    next
}

NF<2 {
    print
    next
} 

{
    varname = $1
    gsub(/ /, "", varname)
    if (varname in var) {
	# try to match entire variable name
	$2 = var[varname]
	delete var[varname]
    } else {
	for (key in var) {
	    gsub(key, var[key], $2)
	}
    }
    print
}
