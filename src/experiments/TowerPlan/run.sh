#!/bin/bash
# Works with commit e5e1b8b77be9a6e0b3791b6592a0841084bbbf7f

# Make data directory
mkdir data

# CD to src/ directory
cd ../../

make clean
make tower

# Run script with default parameters
./Strappy/Tower 0 p 0.015 0.1 500 60 5 'experiments/TowerPlan/data' | tee experiments/TowerPlan/data/log