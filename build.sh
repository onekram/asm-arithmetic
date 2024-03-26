#!/bin/bash
set -ou pipefail

mkdir build
cd build || exit
cmake ..
make
cd ..
