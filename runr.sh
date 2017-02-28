#!/bin/bash

# Run the bash script
module load R_earth/3.1.0
R --slave --vanilla --quiet --no-save  <<EEE
source('/usr3/graduate/xjtang/Documents/tsvst/nobpm.R')
$1($2,$3,$4)
EEE
