library(DBI)

setwd("~/UEF_projects/ninni/")
source("app/www/db_functions.R")

db_info <- read_db_info("app/www/database_www.config")

con <- dbConnect(
  drv = RPostgres::Postgres(),
  dbname = db_info$db_name,
  host = db_info$db_host,
  port = db_info$db_port,
  user = db_info$db_user,
  password = db_info$db_password)

# orig <- list()
# for (tbl in dbListTables(con)) {
#   orig[[tbl]] <- dbReadTable(con, tbl)
# }
# 
# save(orig, file = "app/orig_data.RData")



# Real code
append <- FALSE


if (append) {
  
  # Check if datasets exist in database, skip those
  
  ids <- get_last_ids(con)
} else {
  execute_sql_file("src/drop_schema.sql", con)
  cat("Dropped database schema\n")
  execute_sql_file("src/create_schema.sql", con)
  cat("Created database schema\n")
  
  # Initialize all last IDs to 0
  ids <- sapply(dbListTables(con), function(x) {0})
}


# Write old data back for demoing Ninni
# load(file = "app/orig_data.RData")
# for (tbl in names(orig)) {
#   tryCatch({
#     dbAppendTable(con, tbl, orig[[tbl]])
#   }, error = function(e) {print(e$message)})
#   
# }

# Helper function for appending data to database
append_table <- function(df, tbl) {
  if (!"id" %in% colnames(df)) {
    df$id <- seq_len(nrow(df)) + ids[tbl]
  }
  colnames(df) <- tolower(colnames(df))
  dbAppendTable(con, tbl, df)
}

# Import dataset metadata
metadata_old <- dbReadTable(con, "datasetmetadata")
metadata <- read.csv("example_data/metadata.csv", stringsAsFactors = FALSE)
colnames(metadata) <- tolower(colnames(metadata))

# Remove metadata labels already in the database
metadata <- metadata[!metadata$label %in% metadata_old$label, ]
append_table(metadata, "datasetmetadata")

# Import datasets
datasets <- read.csv("example_data/datasets.csv", stringsAsFactors = FALSE)
datasets$DATASET_FILENAME <- gsub("[.][.]/", "", datasets$DATASET_FILENAME)
datasets$VARIABLES_FILENAME <- gsub("[.][.]/", "", datasets$VARIABLES_FILENAME)

# Check that files exist
check_files(datasets)

# Import datasets
colnames(datasets) <- tolower(colnames(datasets))

assocs_full <- lapply(datasets$dataset_filename, read.csv, stringsAsFactors = FALSE) 

datasets$rowcount <- sapply(assocs_full, nrow)
datasets$id <- seq_len(nrow(datasets)) + ids["datasets"]
dbAppendTable(con, "datasets", datasets[c("id", "label", "rowcount", "description", "varnum", "effect_type")])

# Dataset to metadata
# Read the whole metadata table
metadata <- dbReadTable(con, "datasetmetadata")
ds2md <- data.frame()
for (i in seq_len(nrow(datasets))) {
  meta_labels <- strsplit(datasets$metadata_labels[i], split = ";")[[1]]
  for (label in meta_labels) {
    ds2md <- rbind(ds2md, data.frame(datasetmetadata_id = metadata$id[metadata$label == label],
                                     dataset_id = datasets$id[i]))
  }
}
append_table(ds2md, "datasettometadata")


# Import associations
variables_old <- variables_all <-  dbReadTable(con, "variables")
metavariables_old <- metavariables_all <- dbReadTable(con, "metavariables")
# Initialize new data frames to import
variables <- metavariables <- associations <- assoc2var <- numval <- strval <- data.frame()

# Add stuff to be imported from each dataset
for (i in seq_len(nrow(datasets))) {
  # Read variable descriptions
  var_fname <- datasets$variables_filename[i]
  if (i == 2) var_fname <- ""
  if (!is.na(var_fname) && var_fname != "") {
    variables_tmp <- read.csv(var_fname, stringsAsFactors = FALSE)
    colnames(variables_tmp) <- tolower(colnames(variables_tmp))
  } else {
    variables_tmp <- data.frame()
  }
  
  # Find variables with only a dummy description in the database
  dummy_vars <- variables_old[variables_old$label %in% variables$label, ]
  dummy_vars <- dummy_vars[dummy_vars$label == dummy_vars$description, ]
  if (nrow(dummy_vars)) {
    cat(paste("Updating the description of", nrow(dummy_vars), "variables\n"))
    dummy_vars$description <- NULL
    dummy_vars <- dplyr::left_join(dummy_vars, variables, by = "label")
    # Update their descriptions with the new descriptions
    for (j in seq_len(nrow(dummy_vars))) {
      dbSendQuery(con, statement = paste0("UPDATE variables set description = '", dummy_vars$description[j],
                                          "' WHERE id = ", dummy_vars$id[j]))
    }
  }
  # Define usual columns and list of current variables
  assoc <- assocs_full[[i]]
  varnum <- datasets$varnum[i]
  if (varnum == 1){
    normal_columns = c("VARIABLE1_LABEL", "EFFECT", "EFFECT_L95", "EFFECT_U95", "N", "P", "P_FDR")
    # Variables to import
    var_labels <- unique(assoc$VARIABLE1_LABEL)
  } else {
    normal_columns = c("VARIABLE1_LABEL", "VARIABLE2_LABEL", "EFFECT", "EFFECT_L95", "EFFECT_U95", "N", "P", "P_FDR")
    # Variables to import
    var_labels <- unique(c(assoc$VARIABLE1_LABEL, assoc$VARIABLE2_LABEL))
  }
  
  # Add dummy descriptions for variables without actual description
  new_vars <- setdiff(var_labels, variables_tmp$label)
  variables_tmp <- rbind(variables_tmp, data.frame(label = new_vars, description = new_vars,
                                           stringsAsFactors = FALSE))
  # Remove variables already in the database and add them to the variables to be appended
  variables_tmp <- variables_tmp[!variables_tmp$label %in% variables_all$label, ]
  variables_tmp$id <- seq_len(nrow(variables_tmp)) + ids["variables"]
  ids["variables"] <- ids["variables"] + nrow(variables_tmp)
  variables <- rbind(variables, variables_tmp)
  variables_all <- rbind(variables_all, variables_tmp)
  # Take IDs for all variables in current set, even previously found
  # These are needed when linking variables with associations
  variables_tmp <- variables_all[variables_all$label %in% var_labels, ]
  rownames(variables_tmp) <- variables_tmp$label
  
  # New associations
  
  associations_tmp <- assoc[normal_columns]
  associations_tmp$Dataset_ID <- datasets$id[i]
  associations_tmp$id <- seq_len(nrow(associations_tmp)) + ids["associations"]
  ids["associations"] <- ids["associations"] + nrow(associations_tmp)
  rownames(assoc) <- associations_tmp$id
  associations <- rbind(associations, associations_tmp[c("id", "Dataset_ID", "EFFECT", "EFFECT_L95", "EFFECT_U95", "N", "P", "P_FDR")])
  # Link associations to variables
  assoc2var_tmp <- data.frame(association_id = associations_tmp$id,
                              variable_id = variables_tmp[associations_tmp$VARIABLE1_LABEL, "id"])
  if (varnum == 2) {
    assoc2var_tmp <- rbind(assoc2var_tmp,
                           data.frame(association_id = associations_tmp$id,
                                      variable_id = variables_tmp[associations_tmp$VARIABLE2_LABEL, "id"]))
  }
  assoc2var <- rbind(assoc2var, assoc2var_tmp)
  
  # Check if there are extra columns (metavariables)
  # If yes, populate metavariables table and save types metavariables to a named vector
  if (ncol(assoc) > length(normal_columns)) {
    # Get the names of metavariables
    metavar_labels <- setdiff(colnames(assoc), normal_columns)
    # Add new metavariables to metavariables table
    metavariables_tmp <- data.frame(label = setdiff(metavar_labels, metavariables_all$label),
                                    stringsAsFactors = FALSE)
    metavariables_tmp$id <- seq_len(nrow(metavariables_tmp)) + ids["metavariables"]
    ids["metavariables"] <- ids["metavariables"] + nrow(metavariables_tmp)
    metavariables <- rbind(metavariables, metavariables_tmp)
    metavariables_all <- rbind(metavariables_all, metavariables_tmp)
    # Take IDs for all metavariables in current set, even previously found
    metavariables_tmp <- metavariables_all[metavariables_all$label %in% metavar_labels, ]
    rownames(metavariables_tmp) <- metavariables_tmp$label
    # Determine types of metavariables in this dataset
    # NOTE: same metavariable can have different types of values in different datasets
    metavariables_tmp$type <- sapply(metavar_labels, function(x) {
      if (is.numeric(assoc[, x])) {
        "num"
      } else {
        "str"
      }
    })
    # Link associations to metavariables
    numval <- rbind(numval, link_metavars(associations_tmp, assoc, type = "num"))
    strval <- rbind(strval, link_metavars(associations_tmp, assoc, type = "str"))
  }
  
}

# Append variables
dfs <- list(variables = variables,
            metavariables = metavariables,
            associations = associations,
            associationtovariable = assoc2var,
            numval = numval,
            strval = strval)
for (i in seq_along(dfs)) {
  append_table(dfs[[i]], names(dfs)[i])
}


dbDisconnect(con)
