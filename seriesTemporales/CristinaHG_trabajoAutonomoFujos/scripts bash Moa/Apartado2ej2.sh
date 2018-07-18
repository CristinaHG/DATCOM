#!/bin/bash

for i in `seq 1 20`;
do
   java -cp moa.jar -javaagent:sizeofag-1.0.0.jar moa.DoTask \
   "EvaluateInterleavedTestThenTrain -l trees.HoeffdingAdaptiveTree -s \
   (generators.WaveformGenerator -i $i) -i 1000000 -f 10000" \
   > ./Ej1Res/Apartado2ejercicio2semilla$i.txt 
done

 
