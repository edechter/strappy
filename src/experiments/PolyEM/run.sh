#!/bin/bash

# Make data directories for each frontier size
mkdir data1000
mkdir data5000
mkdir data10000

# CD to src/ directory
cd ../../

# Compile polynomial regression
make clean
make poly

# Run script w/ EM
./Strappy/Poly 0 1.5 1.0 1000 'experiments/PolyEM/data1000' | tee experiments/PolyEM/data1000/log
./Strappy/Poly 0 1.5 1.0 5000 'experiments/PolyEM/data5000' | tee experiments/PolyEM/data5000/log
./Strappy/Poly 0 1.5 1.0 10000 'experiments/PolyEM/data10000' | tee experiments/PolyEM/data10000/log