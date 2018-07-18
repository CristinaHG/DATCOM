#!/bin/bash

for i in `seq 1 20`;
do
   java -cp moa.jar -javaagent:sizeofag-1.0.0.jar moa.DoTask \
   "EvaluateInterleavedTestThenTrain -l trees.HoeffdingTree \
   -s (generators.RandomRBFGeneratorDrift -s 0.001 -k 3 -r $i -i $i -a 7 -n 3) \
   -i 2000000" > ./Ej1Res/Apartado3ejercicio1semilla$i.txt 
done


