# replace a line in the file with a content of another file
# Usage:
# echo -e "1 1 1 2\n1 3 4 10" | awk -f xyz2col.awk  > xyz
# awk -v tmpfile=xyz -v line=COLL_SHAPE -f line2file.awk physics_config.m4

$0==line{
    while ( (getline var < tmpfile ) > 0) {
	print var
    }
    next
}

1
