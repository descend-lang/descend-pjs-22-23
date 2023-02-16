#!/bin/bash
set -e
set -o pipefail
# set -x

module load palma/2021a
module load CUDA
module load foss/2021a

export PLAT="rtx3090"

mkdir -p ./runs/vector_add/

for i in {1..6}
do 
    for j in {1..2}
    do 
        # wg=$(( i*16 ))
        wg=$(python3 -c "print(2**(3+$i))")
        th=$(python3 -c "print(2**(8+$j))")
        echo "wg: $wg, th: $th"
        sed -i "s/WG XX/WG $wg/g" vector-add.cu
        sed -i "s/THREADS XX/THREADS $th/g" vector-add.cu
        nvcc ./runs/vector_add/vector-add.cu -o ./runs/vector_add/vector_add_${PLAT}_${wg}_${th}.out --extended-lambda
        sed -i "s/WG $wg/WG XX/g" vector-add.cu
        sed -i "s/THREADS $th/THREADS XX/g" vector-add.cu
        echo "successfully compiled for wg: $wg and th: $th"
    done
done
