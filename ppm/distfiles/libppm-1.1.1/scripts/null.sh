#! /bin/bash

set -u
for f in *.F90; do
    ntotal=$(awk '/DIMENSION.*POINTER[ ][ ]*::/' ${f} | wc -l)
    printf "processing: %s\n" ${f}
    printf "npointers: %s\n" ${ntotal}
    for n in $(seq 1 ${ntotal}); do
	cp ${f} ${f}.bak
	awk -v n=${n} '/DIMENSION.*POINTER[ ][ ]*::/{i++; 
                         if (i==n) {
                              split($0, aux, "::")
                              vars = aux[2]
                              gsub(",", "=>null(),", vars)
                              print aux[1]"::"vars"=>null()"
                             } else {
                              print
                             }
                            next
                      } 1' ${f}.bak > ${f}
	# try to build
	make > make.log
	# if fail restore from a backup
	if [ $? -ne 0 ]; then
	    cp ${f}.bak ${f}
	fi
    done
done
