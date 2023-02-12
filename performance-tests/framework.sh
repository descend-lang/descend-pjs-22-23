#!/bin/bash
set -e
set -o pipefail
# set -x

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

rm -rf output.log

for i in {1..20}
do 
{ time {
        echo "test"
        sleep 0.1
    } } 2>> output.log
done

python3 "${SCRIPT_DIR}/framework.py" "cuda"
rm -rf output.log