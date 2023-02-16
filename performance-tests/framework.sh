#!/bin/bash
set -e
set -o pipefail
# set -x

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

rm -rf output.log

RUN_NAMESPACE=${1:"vector_add"}
RUN_PROGRAMM=${2:"vector_add_rtx3090_16_1024.out"}

mkdir -p "${SCRIPT_DIR}/${RUN_NAMESPACE}"

for i in {1..500}
do 
{ time {
        echo "./${SCRIPT_DIR}/${RUN_NAMESPACE}/${RUN_PROGRAMM}"
    } } 2>> output.log
done

python3 "${SCRIPT_DIR}/framework.py" "${SCRIPT_DIR}/${RUN_NAMESPACE}/${RUN_PROGRAMM}"
rm -rf output.log