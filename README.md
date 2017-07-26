# Ninni

Ninni is a data visualization web app designed for representing results from scientific studies.

The main focus is in representing effects (odds-ratio, fold change or correlation) of variables (e.g. drugs) or association between variables (e.g. drug interaction) on another variable (e.g. mortality, risk of arrhytmia)

Ninni currently has following interactive plots available:
+ Clustered heatmaps
+ Volcano plots
+ Q-Q plots
+ Lady Manhattan plots

Ninni uses [Shiny](https://shiny.rstudio.com). Ninni's data is stored in a [PostgreSQL](https://www.postgresql.org/) database.

#### Installation and set up

1. Download git repo  
```
git init  
git remote add origin https://github.com/antonmattsson/ninni  
git pull origin master
```

2. Install required software, see `docs/SETUP.txt`

3. Setup a PostgreSQL database for Ninni

4. See information about the database structure and importing data in `docs` folder: `database_structure.txt`, `db_schema.pdf` and `importing_data_instructions.txt`

5. Set up shiny server and run Ninni (instructions coming soon)



*Ninni is licensed under the terms of the MIT license*
