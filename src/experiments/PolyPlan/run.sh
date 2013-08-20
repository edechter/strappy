#!/bin/bash
# Works with commit dd48bca3f6d1d3fd9279bde4387fcac4ba8c9c48

# Make data directories for each frontier size
mkdir data100
mkdir data500

# CD to src/ directory
cd ../../

# Compile polynomial regression
make clean
make poly

# Run script w/ planning
./Strappy/Poly 0 p 1.5 1.0 100 10 3 'experiments/PolyEM/data1000' | tee experiments/PolyPlan/data100/log
./Strappy/Poly 0 p 1.5 1.0 500 10 3 'experiments/PolyEM/data5000' | tee experiments/PolyPlan/data500/log
