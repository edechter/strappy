#!/bin/bash

# Make data directories for each frontier size
mkdir data1000
mkdir data5000
mkdir data10000

# CD to src/ directory
cd ../../

# Run script w/ EM
./Strappy/Poly 1.5 1.0 1000 'experiments/TowerPlan/data1000'
./Strappy/Poly 1.5 1.0 5000 'experiments/TowerPlan/data5000'
./Strappy/Poly 1.5 1.0 10000 'experiments/TowerPlan/data10000'