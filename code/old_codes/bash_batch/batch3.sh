#!/bin/bash
for file in bash/wav/batch3/*.wav
do
   Rscript code/bash_predictions.R ${file}
done