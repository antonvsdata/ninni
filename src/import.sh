#!/bin/bash
printf 'Activating conda environment...\n'
source activate ninni_env
printf 'Environment activated \n'

printf 'Importing data \n \n'
python import_data.py -dsf ../data/datasets.csv -mdf ../data/metadata.csv

printf '\n \nDeactivating conda environment\n'
source deactivate ninni_env
printf 'Environment deactivated \n'
