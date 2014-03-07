# transforms a stream of <x, y, z, radius> to a part of mcf
# configuration file corresponding to colloidal
# Usage:
# echo -e "1 1 1 2\n1 3 4 10" | awk -f xyz2col.awk

NF==4 {
    printf "\n"
    printf "coll_shape = 2\n"
    printf "coll_radius = %e, 0.0, 0.0\n", $4
    printf "coll_x = %e, %e, %e\n", $1, $2, $3
}
