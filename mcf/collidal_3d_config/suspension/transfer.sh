#!/bin/bash

rsync -av -e '/home/litvinov/bin/chain-ssh -t litvinov@yoko ssh' \
    lu79buz2@supermuc.lrz.de:/gpfs/scratch/pr32ma/lu79buz2/N* supermuc-data/

