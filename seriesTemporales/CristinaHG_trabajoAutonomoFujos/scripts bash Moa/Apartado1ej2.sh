#!/bin/bash

for i in `seq 1 20`;
do
   java -cp moa.jar -javaagent:sizeofag-1.0.0.jar moa.DoTask \
   "EvaluateModel -m (LearnModel -l trees.HoeffdingAdaptiveTree -s \
   (generators.WaveformGenerator -i $i) -m 1000000) -s \
   (generators.WaveformGenerator -i 4)" > ./Ej1Res/Apartado1ejercicio2semilla$i.txt 
done

