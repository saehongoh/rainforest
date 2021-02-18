#!/bin/bash
for file in bash/wav/batch5/*.wav
do
   Rscript code/bash_predictions.R ${file}
done