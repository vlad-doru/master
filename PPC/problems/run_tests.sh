#!/bin/bash

echo "CENTRALIZED" 
for file in $( ls input_*.txt ); do
  echo $file
  cp $file input.txt
  mpirun -np 3 ./centralized | grep "Time"
done

echo "DECENTRALIZED" 
for file in $( ls input_*.txt ); do
  echo $file
  cp $file input.txt
  mpirun -np 3 ./decentralized | grep "Time"
done

cp input_9.txt input.txt
