#!/bin/bash
set -e
set -o pipefail
# set -x

# module load palma/2021a
# module load CUDA
# module load foss/2021a

export PROG="vector_add_cl"
export PLAT="rtx4000"

for i in {1..2}
do 
    for j in {1..2}
    do 
        wg=$(python3 -c "print(2**(3+$i))")
        th=$(python3 -c "print(2**(6+$j))")
        ./framework.sh $PROG ${PROG}_${PLAT}_${wg}_${th}.out
    done
done