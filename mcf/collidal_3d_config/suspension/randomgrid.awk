# generate a random grid of colloids
# Usage:
# awk -v step=0.95 -v r=0.9 -v nfail=1000 -v ncol=1000 -v size=8.0 -f randomgrid.awk

# check if a new colloid overlaps with existing ones
function overlap(x, y, z,       i, d2) {
    for (i=1; i<=n; i++) {
	d2 = (xp[i]-x)**2 + (yp[i]-y)**2 + (zp[i]-z)**2
	if (d2<(2*step)^2) {
	    return 1
	}
    }
    return 0
}


BEGIN {
    srand()
    n=0
    nf=0
    while ((n<ncol) && (nf<nfail)) {
	# generate a new colloid
	x = rand()*(size-2*step) + step
	y = rand()*(size-2*step) + step
	z = rand()*(size-2*step) + step

	if (!overlap(x, y, z)) {
	    nf = 0
	    n++
	    xp[n]=x; yp[n]=y; zp[n]=z
	} else {
	    nf++
	}
    }

    if (nf>=nfail) {
	printf "I tried to place a colloid %i times and fail\n", nf > "/dev/stderr"
	printf "%i of %i colloids were generated\n", n, ncol > "/dev/stderr"
    }
    
    # print coordinates
    for (i=1; i<=n; i++) {
	print xp[i], yp[i], zp[i], r
    }
}

