# generate a simple grid
# Usage:
# awk -v r=0.90 -v step=0.95 -v size=8.0 -f simplegrid.awk

BEGIN {
    sizex=sizey=sizez=size
    x = 0.5*step
    while (x+r<sizex) {
	y = 0.5*step
	while (y+r<sizey) {
	    z = 0.5*step
	    while (z+r<sizez) {
		print x, y, z, r
		z += step
	    }
	    y += step
	}
	x += step
    }
}

