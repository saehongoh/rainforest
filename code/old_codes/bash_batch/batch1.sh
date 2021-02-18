#!/bin/bash
for file in bash/wav/batch1/*.wav
do
   Rscript code/bash_predictions.R ${file}
done