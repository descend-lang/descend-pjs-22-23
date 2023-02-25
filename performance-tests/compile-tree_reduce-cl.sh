#!/bin/bash
set -e
set -o pipefail
# set -x

# module load palma/2021a
# module load CUDA
# module load foss/2021a

export PLAT="rtx4000"

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

mkdir -p $SCRIPT_DIR/runs/tree_reduce_cl/

SOURCE_FILE=tree_reduce.cpp
SOURCE_DIR=$SCRIPT_DIR/runs/tree_reduce_cl

# CUDA
for i in {1..2}
do 
    for j in {1..2}
    do 
        # wg=$(( i*16 ))
        wg=$(python3 -c "print(2**(3+$i))")
        th=$(python3 -c "print(2**(6+$j))")
        echo "wg: $wg, th: $th"
        sed -i "s/WG XX/WG $wg/g" $SOURCE_DIR/$SOURCE_FILE
        sed -i "s/THREADS XX/THREADS $th/g" $SOURCE_DIR/$SOURCE_FILE
        cp -r $SOURCE_DIR/$SOURCE_FILE $SOURCE_DIR/tree_reduce_cl_${PLAT}_${wg}_${th}.cpp
        cd $SOURCE_DIR
        cmake .
        cmake --build . --target tree_reduce
        mv tree_reduce tree_reduce_cl_${PLAT}_${wg}_${th}.out
        sed -i "s/WG $wg/WG XX/g" $SOURCE_DIR/$SOURCE_FILE
        sed -i "s/THREADS $th/THREADS XX/g" $SOURCE_DIR/$SOURCE_FILE
        echo "successfully compiled for wg: $wg and th: $th"
    done
done

