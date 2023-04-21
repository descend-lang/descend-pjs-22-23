#!/bin/bash
set -e
set -o pipefail
# set -x

sudo apt update
sudo apt install clinfo
sudo apt install opencl-headers ocl-icd-opencl-dev
sudo apt install nvidia-cuda-toolkit
sudo apt install cmake
sudo apt install nvidia-driver-525

# https://github.com/NVIDIA/nccl/issues/650#issuecomment-1145173577 um Cuda zu reparieren