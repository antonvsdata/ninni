# Ninni

Ninni is a web application designed for representing results from scientific studies.

The results represented by Ninni can be e.g. following:

- The difference in levels of a metabolite between two study groups.
- Effect of drugs to the risk of arrhytmia, measured in odds ratio
- Effect of interaction of drugs to the risk of arrhytmia, measured in odds ratio
- Differential expression of genes in healthy and diseased individuals, measures in fold change
- Correlation of metabolite levels in human metabolome

... or anything else that can be represented as an association between variables and an outcome.

Ninni saves results of multiple studies to a dedicated database. Users can then browse the database and combine results from different studies, download and visualize them.

Currently, Ninni provides the following visualizations:

- Clustered heatmaps
- Volcano plots
- Q-Q plots
- Directed Manhattan plots
- Lollipop plots of most common variables
- Upset plots of common variables between result sets
- P-value histograms
- Ridge (aka Joy) plots
- Network visualizations

Ninni is powered by [Shiny](https://shiny.rstudio.com).

_Ninni is developed at University of Eastern Finland and licensed under the terms of the MIT license_

## Installation and set up

First clone the git repository:

```
git clone https://github.com/antonvsdata/ninni
```

Next, choose a username and password for admin access to Ninni. Admin access allows the user to import and delete data from the database. The information should be stored in app/www/user.config file. See app/www/user.config-TEMPLATE for a template of the format. In the template, the username is ninni, and password is ninni. The password is stored as a hash, computed with the bcrypt R package. To compute the hash for your password, run:

```
if (!requireNamespace("bcrypt", quietly = TRUE)) {
  install.packages("bcrypt)
}
bcrypt::hashpw("your_password")
```

To continue, you have two options: host Ninni inside a docker container with Shiny Server, or try it out as a local Shiny app

### Option 1: Docker container

Ninni is dockerized, so you can build a docker image and start Ninni inside a docker container.

Navigate to the root of the Ninni repository and build the docker image (this will take a while):

```
docker build -t ninni_app .
```

Next, create a docker volume. The volume is used to store the database of Ninni. Storing the database inside a volume ensures that the database is kept intact even if the docker container needs to be restarted.

```
docker volume create ninni_db_vol
```

Next, run the container and mount the volume:

```
docker run -d --name ninni_app -p 90:90 --mount source=ninni_db_vol,target=/srv/shiny-server/db ninni_app
```

NOTE: the port number 90 is defined in the Dockerfile, if you want Ninni to run in a different port, feel free to change it there and also in the command above.

### Option 2: Run Ninni locally

To set up Ninni's database you need to install SQLite. The Shiny web app uses R 3.6.3 (but it should work with R 4.0.x). You can install all the required R packages with `docker/install.packages.R` script. You can then run Ninni by running the following command in R:

```
shiny::runApp("path/to/ninni/app")
```

#### Import data to the database

If you have set up an admin user (see above), you can import data to the Ninni database through the "Admin" tab. This section describes the format of the data.

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

When all the files are in the right format, you can import the datasets via Ninni.

#### 5. Shiny app

Once you have imported data into your database and added connection information you can run a local version
of the Shiny application from R console with the command:  
`shiny::runApp("path/to/ninni/app")`

_NOTE Ninni currently displays warnings related to plotly widget IDs and ggplot aesthetics. These warnings can be ignored_

If you want to deploy the Shiny app publicly to the web use Shiny Server. Instructions are available online.
