#!/bin/bash
echo activating conda environment
source activate ninnni_env
echo importing data
python import_data.py -dsf ../data/datasets.csv -mdf ../data/metadata.csv
echo deactivate conda environment
source deactivate ninni_env
