#!/bin/bash
set -e
set -o pipefail
# set -x

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
RUN_NAMESPACE=${1:-vector_add}
RUN_PROGRAMM=${2:-vector_add_rtx3090_16_1024.out}

rm -rf output.log

echo $RUN_NAMESPACE
echo $RUN_PROGRAMM

mkdir -p "${SCRIPT_DIR}/${RUN_NAMESPACE}"

for i in {1..20}
do 
    "${SCRIPT_DIR}/runs/$RUN_NAMESPACE/$RUN_PROGRAMM" >> output.log
done

python3 "${SCRIPT_DIR}/framework-cu.py" "$RUN_PROGRAMM"
mv "./$RUN_PROGRAMM.csv" "./runs/$RUN_NAMESPACE/"
rm -rf output.log