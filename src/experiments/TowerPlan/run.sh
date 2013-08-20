#!/bin/bash
# Works with commit dd48bca3f6d1d3fd9279bde4387fcac4ba8c9c48

# Make data directory
mkdir data

# CD to src/ directory
cd ../../

make clean
make tower

# Run script with default parameters
./Strappy/Tower 0 p 0.015 0.1 500 60 5 'experiments/TowerPlan/data' | tee experiments/TowerPlan/data/log