#!/bin/bash

# Make data directory
mkdir data

# CD to src/ directory
cd ../../

make clean
make tower

# Run script with default parameters
./Strappy/Tower 0 0.015 0.1 500 60 5 'experiments/TowerPlan/data' | tee experiments/TowerPlan/data/log