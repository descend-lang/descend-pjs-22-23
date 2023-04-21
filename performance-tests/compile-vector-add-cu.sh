#!/bin/bash
set -e
set -o pipefail
# set -x

# module load palma/2021a
# module load CUDA
# module load foss/2021a

export PLAT="rtx4000"

mkdir -p ./runs/vector_add_cu/

SOURCE_FILE=vector-add.cu
SOURCE_DIR=./runs/vector_add_cu

# CUDA
for i in {1..4}
do 
    for j in {1..4}
    do 
        # wg=$(( i*16 ))
        wg=$(python3 -c "print(2**(3+$i))")
        th=$(python3 -c "print(2**(6+$j))")
        echo "wg: $wg, th: $th"
        sed -i "s/WG XX/WG $wg/g" $SOURCE_DIR/$SOURCE_FILE
        sed -i "s/THREADS XX/THREADS $th/g" $SOURCE_DIR/$SOURCE_FILE
        nvcc $SOURCE_DIR/$SOURCE_FILE -std=c++14 -o $SOURCE_DIR/vector_add_cu_${PLAT}_${wg}_${th}.out --extended-lambda
        sed -i "s/WG $wg/WG XX/g" $SOURCE_DIR/$SOURCE_FILE
        sed -i "s/THREADS $th/THREADS XX/g" $SOURCE_DIR/$SOURCE_FILE
        echo "successfully compiled for wg: $wg and th: $th"
    done
done

