# Ninni

Ninni is a web application designed for representing results from scientific studies.

The results represented by Ninni can be e.g. following:

- Effect of drugs to the risk of arrhytmia, measured in odds ratio
- Effect of interaction of drugs to the risk of arrhytmia, measured in odds ratio
- Differential expression of genes in healthy and diseased individuals, measures in fold change
- Correlation of metabolite levels in human metabolome

... or anything else that can be represented as an association between variables.

Ninni saves results of multiple studies to a dedicated database. Users can then browse the database and combine results from different studies, download and visualize them.

Currently, Ninni provides the following visualizations:

- Clustered heatmaps
- Volcano plots
- Q-Q plots
- Lady Manhattan plots

Ninni is powered by [Shiny](https://shiny.rstudio.com).

_Ninni is licensed under the terms of the MIT license_

## Installation and set up

#### 1. Download git repo

```
git init
git remote add origin https://github.com/antonmattsson/ninni
git pull origin master
```

#### 2. Install required software

##### PostgreSQL

To set up Ninni's database you need to install PostgreSQL (we are using version 9.2.18 for Red Hat family Linux).

##### Python

The import script to Ninni's database was written in Python 3.5.3 and requires the psycopg2 library (v2.7.1) to connect to a postgreSQL database.

You can install these prerequisites manually or use the conda environment file in the src folder.
If you dont have conda installed, you can get it from here: https://conda.io/docs/install/quick.html.

Once you have conda installed, you can install the ninni environment using src/ninni_env.json:  
`$ conda env create -n ninni_env –f ninni_env.json`

The correct version of Python and the psycopg2 library will be installed. The conda environment can also be used to run the script without having to worry about compatibility issues (see later).

##### R

The Shiny web app uses R 3.3.3. You can install all the required R packages with `src/install.packages.R` script.
Unfortunately it is not possible to install R 3.3.3 nor these packages through conda.

###### Shiny Server

To be able to host a Shiny application, you need to install Shiny Server (or Shiny Server Pro).

#### 3. Set up the database

Set up a PostgreSQL database add the connection information to following files:  
`src/database_import.config`  
`www/database_www.config`

You can see the correct format from example files  
`src/database_import.config-TEMPLATE` and `www/database_www.config-TEMPLATE`

The reason for having two files is that in our case we have two users for the database. One has both reading and writing permissions
and is used by the import script. The other has only reading permissions and is used by the Shiny application. You can also use the same user for both if you prefer.

By default, the import script drops and recreates the database schema in the beginning of every import. Thus, there is no need to create any
tables prior to import of data, but the schema will be created automatically. For detailed structure of the database schema, see `docs/database_structure.txt` and `docs/db_schema.pdf`.

#### 4. Import data to the database

##### Result files

All the datasets you wish to import to Ninni's database should be in .csv files with the following columns (in this order):

VARIABLE1_LABEL: The label ofthe first variable  
VARIABLE2_LABEL: The label of the second variable (exclude this column if the dataset has only one variable)  
EFFECT  
EFFECT_L95: lower end of 95% confidence interval  
EFFECT_U95: upper end of 95% confidence interval  
N: sample size  
P: p-value  
P_ADJ: adjusted p-value

Plus any number of additional columns

Example: `study1.csv`

VARIABLE1_LABEL,VARIABLE2_LABEL,EFFECT,EFFECT_L95,EFFECT_U95,N,P,P_ADJ,CLASS  
ABCD,TP53,8,5,10,100,0.01,0.2,A

_NOTE: Any additional columns will be imported as numeric values if they look like numeric values in R_

##### Variable files

You can add a .csv file describing the variables.
If this file is not specified for the dataset, variable description is set to be the same as the variable label.
The file should have the following columns:

LABEL: The variable label, same as in the previous csv file
DESCRIPTION: Description of the variable

Example: `study1_variables.csv`

LABEL,DESCRIPTION  
ABCD,Best known gene to man-kind  
TP53,Cancer gene

##### List datasets

The datasets should be listed in a .csv file with the following columns:

DATASET_FILENAME: the name of the .csv file containing the dataset  
VARIABLES_FILENAME: optional file describing the variables  
LABEL: short label for the dataset  
DESCRIPTION: longer description of the dataset  
VARNUM: number of variables per association in the dataset (1 or 2)  
EFFECT_TYPE: The effect type in the dataset: "OR" for odds-ratio, "FC" for fold-change or "CORR" for correlation  
METADATA_LABELS: possible metadata labels for the dataset, separated by ';'

_NOTE: it is recommended to use relative path from src folder as filenames, since the import script will most likely be run from the src folder_

Example: `datasets.csv`

DATASET_FILENAME,VARIABLES_FILENAME LABEL,DESCRIPTION,VARNUM,EFFECT_TYPE,METADATA_LABELS
../data/study1_results.csv,../data/study1_variables.csv,DRUG_STUDY1,Drug interaction study using mortality as outcome,2,OR,DRUG_INTERACTION;MALES;T2D

_NOTE: there is only one line of data in the above file_

##### Dataset metadata

Metadata labels of datasets should be described in a separate .csv file with the following columns:

LABEL: The metadata label, same as in the previous .csv file listing datasets  
DESCRIPTION: Description of the metadata label

Example: `metadata.csv`

LABEL,DESCRIPTION  
DRUG_INTERACTION,Drug interaction study  
MALE,Only males  
T2D,Type 2 Diabetes

##### Importing data

When all the files are in the right format, you can import the datasets using a GUI made in Shiny.
This complementary app is found in the `import_app` folder and can be started with the command:
`shiny::runApp("path/to/ninni/import_app")`

#### 5. Shiny app

Once you have imported data into your database and added connection information you can run a local version
of the Shiny application from R console with the command:  
`shiny::runApp("path/to/ninni/app")`

_NOTE Ninni currently displays warnings related to plotly widget IDs and ggplot aesthetics. These warnings can be ignored_

If you want to deploy the Shiny app publicly to the web use Shiny Server. Instructions are available online.
