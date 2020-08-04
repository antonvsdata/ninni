#' Read inforamtion for connecting to the database
#' 
#' Read a config file of specified format for information required to connect to the local PostgreSQL database
#' such as hostname, port, username and password
#' 
#' @param config_file path to the configuration file
#' 
#' @return named list of configuration information
#' 
read_db_info <- function(config_file){
  df <- read.table(config_file)
  
  l <- as.list(as.character(df$V2))
  names(l) <- df$V1
  l
}

#' Read SQL statements from a file
#' 
#' Reads an SQL statement from a text file, and parses it. Used by execute_sql_file
#' 
#' @param file path to the file
#' 
#' @return character containing the SQL statement
#' 
get_sql <- function(file){
  conn <- file(file, "r")
  sql_string <- ""
  
  while (TRUE){
    line <- readLines(conn, n = 1)
    
    if ( length(line) == 0 ){
      break
    }
    
    line <- gsub("\\t", " ", line)
    
    if(grepl("--",line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }
    
    sql_string <- paste(sql_string, line)
  }
  
  close(conn)
  
  sql_string <- strsplit(sql_string, split = ";")[[1]]
  sql_string <- trimws(sql_string)
  
  return(sql_string)
}

#' Execute SQL statement from file
#' 
#' Reads an SQL statement from a file and executes that statement
#' 
#' @param file path to the file
#' @param con a database connection object as returned by dbConnect
execute_sql_file <- function(file, con) {
  
  sql <- get_sql(file)
  
  for (query in sql) {
    q <- dbSendQuery(con, statement = query)
    dbClearResult(q)
  }
  
}

#' Which files are missing
#' 
#' Given a filename, checks if the file exist and returns the file name if missing
#' 
#' @param x file name
#' @param dir directory to search for the file
#' 
#' @return input file name if missing, NA if the file exists
get_missing <- function(x, dir = "data/") {
  if (x == "" || is.na(x)) {
    return(NA_character_)
  }
  x <- paste0(dir, x)
  if (!file.exists(x)) {
    x
  } else {
    NA_character_
  }
}

#' HTML error message
#' 
#' Creates an HTML paragraph for an error message
#' 
#' @param messages the error messages to show
#' 
#' @return an HTML element showing the error messages
error_html <- function(messages) {
  HTML(paste('<p style = "color: red;">', messages, '</p>'))
}

#' Check that all listed files exist
#' 
#' Checks that files to be imported exist
#' 
#' @param datasets data frame of dataset information, including paths to the .csv files
#' @param metadata data frame of dataset metadata information
#' 
#' @return NULL if everything is ok, an HTML element with error message if problems are detected
check_files <- function(datasets, metadata) {
  
  # Check datasets
  msgs <- c()
  dataset_cols <- c("DATASET_FILENAME", "VARIABLES_FILENAME", "LABEL",
                    "DESCRIPTION", "VARNUM", "EFFECT_TYPE", "METADATA_LABELS")
  if(!identical(colnames(datasets), dataset_cols)) {
    msgs <- c(msgs,
              "Column names of the datasets file do not match the template",
              paste("Variables missing:", paste0(setdiff(dataset_cols, colnames(datasets)), collapse = ",")),
              paste("Unknown variables:", paste0(setdiff(colnames(datasets), dataset_cols), collapse = ",")))
  }
  # Check metadata
  metadata_cols <- c("LABEL", "DESCRIPTION")
  if(!identical(colnames(metadata), metadata_cols)) {
    msgs <- c(msgs,
              "Column names of the metadata file do not match the template",
              paste("Variables missing:", setdiff(metadata_cols, colnames(metadata))),
              paste("Unknown variables:", setdiff(colnames(metadata), metadata_cols)))
  }
  
  weird_varnum <- setdiff(unique(datasets$VARNUM), c(1, 2))
  if (length(weird_varnum)) {
    msgs <- c(msgs,
              "VARNUM column of datasets file should only contain values 1 or 2")
  }
  
  md_labels <- paste0(datasets$METADATA_LABELS, collapse = ";")
  md_labels <- strsplit(md_labels, split = ";")[[1]]
  weird_md_labels <- setdiff(md_labels, metadata$LABEL)
  if (length(weird_md_labels)) {
    msgs <- c(msgs,
              "The following metadata labels appear in the datasets file but are not listed in the metadata file:",
              weird_md_labels)
  }
  
  if (all(c("DATASET_FILENAME", "VARIABLES_FILENAME") %in% colnames(datasets))) {
    missing_files <- sapply(c(datasets$DATASET_FILENAME, datasets$VARIABLES_FILENAME), get_missing)
    missing_files <- unique(missing_files[!is.na(missing_files)])
    if (length(missing_files)) {
      msgs <- c(msgs,
                "The following files are listed in datasets file but not found:",
                paste0(missing_files, collapse = ", "))
      
    }
  }
  
  if (length(msgs)) {
    error_html(msgs)
  } else {
    NULL
  }
}

#' Get last IDs in tables
#' 
#' Get last ID used in all the tables of the database, or 0 if the table is empty
#' 
#' @param con a database connection object as returned by dbConnect
#' 
#' @return a named vector of the ids
get_last_ids <- function(con) {
  sapply(dbListTables(con), function(x){
    max(dbReadTable(con, x)$id, 0)
  })
}

#' Link metavariables to associations
#' 
#' Creates a table linking metavariables and associations
#' 
#' @param assoc_ids a vector of association ids
#' @param metavariables_tmp a data frame of information on the metavariables of the current dataset
#' @param assoc a data frame of associations
#' @param type either "num" or "str", for the type of metavariables to link
#' 
#' @return data frame with association id, metavariable id and value
#'  
link_metavars <- function(assoc_ids, metavariables_tmp, assoc, type) {
  tmp_vars <- metavariables_tmp$label[metavariables_tmp$type == type]
  
  val_tmp <- expand.grid(metavariable_label = tmp_vars,
                            association_id = assoc_ids,
                            stringsAsFactors = FALSE)
  
  val_tmp$metavariable_id <- metavariables_tmp[val_tmp$metavariable_label, "id"]
  val_tmp$value <- as.vector(t(assoc[, tmp_vars]))
  
  val_tmp$metavariable_label <- NULL
  val_tmp
}

#' Helper function for appending data to database
#' 
#' Appends a dataframe to a table in the database
#' 
#' @param con a database connection object as returned by dbConnect
#' @param df the data frame to append
#' @param tbl the name of the table in the database
#' @param ids a named vector of last ids in the database
append_table <- function(con, df, tbl, ids) {
  if (!"id" %in% colnames(df)) {
    df$id <- seq_len(nrow(df)) + ids[tbl]
  }
  colnames(df) <- tolower(colnames(df))
  dbAppendTable(con, tbl, df)
}

#' Import metadata
#' 
#' Imports dataset metadata
#' 
#' @param con a database connection object as returned by dbConnect
#' @param metadata data frame, the metadata to import
#' @param ids a named vector of last ids in the database
import_metadata <- function(con, metadata, ids) {
  metadata_old <- dbReadTable(con, "datasetmetadata")
  metadata <- metadata[!metadata$LABEL %in% metadata_old$label, ]
  append_table(con, df = metadata, tbl = "datasetmetadata", ids = ids)
}

#' Import dataset information
#' 
#' Imports core dataset information
#' 
#' @param con a database connection object as returned by dbConnect
#' @param datasets data frame, the dataset info to import
#' @param ids a named vector of last ids in the database
import_ds2md <- function(con, datasets, ids) {
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
  append_table(con, ds2md, "datasettometadata", ids)
}

#' Update description of dummy variables
#' 
#' If variables are already in the database, but their description is
#' equal to their label, they are "dummy variables". If a description is 
#' given later, this updates the description in the database.
#' 
#' @param con a database connection object as returned by dbConnect
#' @param variables data frame, new information on variables
#' @param variables_old data frame, exisintg variables 
update_dummy_vars <- function(con, variables, variables_old) {
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
}

#' Import associations
#' 
#' Imports associations to the database and links them to datasets,
#' variables and metavariables. 
#' 
#' @param con a database connection object as returned by dbConnect
#' @param datasets data frame, dataset information
#' @param ids a named vector of last ids in the database
#' @param progress a progress object for the UI
#' 
import_associations <- function(con, datasets, ids, progress) {
  report(progress, "Reading datasets into right format", value = 0.15)
  # Import associations
  variables_old <- variables_all <-  dbReadTable(con, "variables")
  metavariables_old <- metavariables_all <- dbReadTable(con, "metavariables")
  # Initialize new data frames to import
  variables <- metavariables <- associations <- assoc2var <- numval <- strval <- data.frame()
  
  # Add stuff to be imported from each dataset
  for (i in seq_len(nrow(datasets))) {
    # Read variable descriptions
    var_fname <- datasets$variables_filename[i]
    if (!is.na(var_fname) && var_fname != "") {
      variables_tmp <- read.csv(paste0("data/", var_fname), stringsAsFactors = FALSE)
      colnames(variables_tmp) <- tolower(colnames(variables_tmp))
      # Update dummy variable descriptions in the database
      update_dummy_vars(con, variables = variables_tmp, variables_old)
    } else {
      variables_tmp <- data.frame()
    }
    
    # Define usual columns and list of current variables
    assoc <- read.csv(paste0("data/", datasets$dataset_filename[i]), stringsAsFactors = FALSE)
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
      numval <- rbind(numval, link_metavars(associations_tmp$id, metavariables_tmp, assoc, type = "num"))
      strval <- rbind(strval, link_metavars(associations_tmp$id, metavariables_tmp, assoc, type = "str"))
    }
    
  }
  
  # Append variables
  dfs <- list(variables = variables,
              metavariables = metavariables,
              associations = associations,
              associationtovariable = assoc2var,
              numval = numval,
              strval = strval)
  report(progress, "Importing data to database", value = 0.2)
  n <- length(dfs)
  steps <- seq(0.21, 0.95, length.out = n)
  details <- c(paste("Importing", c("variables", "additional information", "associations")),
               paste("Linking associations to", c("main variables",
                                                  "additional numeric information",
                                                  "additional text information")))
  for (i in seq_along(dfs)) {
    report(progress, "Importing data to database", steps[i], detail = details[i])
    append_table(con, dfs[[i]], names(dfs)[i], ids)
  }
  report(progress, "Importing data to database", 0.95, detail ="")
}

#' Report progress
#' 
#' Uses the shiny progress tool to report progress of importing data.
#' 
#' @param progress a progress object for the UI
#' @param msg the message to show
#' @param value a value for the progress bar, between 0 and 1
#' @param detail the minor text to display 
report <- function(progress, msg, value, detail = NULL) {
  if (!is.null(progress)) {
    progress$set(message = msg, detail = detail, value = value)
  }
}

#' Import data
#' 
#' The main function for importing data, calls the other functions.
#' 
#' @param con a database connection object as returned by dbConnect
#' @param datasets data frame, dataset information
#' @param metadata data frame, the metadata to import
#' @param clear logical, whether the database should be cleaned before importing this data.
#' @param progress a progress object for the UI
import_data <- function(con, datasets, metadata, append, progress = NULL) {
  colnames(datasets) <- tolower(colnames(datasets))
  
  if (!clear) {
    
    # Check if datasets exist in database, skip those
    old_datasets <- dbReadTable(con, "datasets")
    datasets <- datasets[!datasets$label %in% old_datasets$label, ]
    
    if (nrow(datasets) == 0) {
      return(FALSE)
    }
    
    ids <- get_last_ids(con)
  } else {
    execute_sql_file("drop_schema.sql", con)
    cat("Dropped database schema\n")
    execute_sql_file("create_schema.sql", con)
    cat("Created database schema\n")
    
    # Initialize all last IDs to 0
    ids <- sapply(dbListTables(con), function(x) {0})
  }
  
  # Import dataset metadata
  report(progress, "Importing metadata", 0.05)
  import_metadata(con, metadata, ids)
  
  report(progress, "Importing dataset information", 0.08)
  
  # Record rowcount and ID for datasets
  datasets$rowcount <- 0
  datasets$id <- seq_len(nrow(datasets)) + ids["datasets"]
  # Import datasets
  dbAppendTable(con, "datasets", datasets[c("id", "label", "rowcount", "description", "varnum", "effect_type")])
  
  # Import dataset to metadata
  import_ds2md(con, datasets, ids)
  
  report(progress, "Importing main data", 0.1)
  # Import associations
  import_associations(con, datasets, ids, progress)
  
  return(TRUE)
}

#' Format ime from seconds to a nice string
#' 
#' @param t time in seconds
#' 
#' @return a string with days, hours, minutes and seconds
format_time <- function(t){
  t <- round(t)
  paste0(t %/% (60*60*24), " d ",
         formatC(t %/% (60*60) %% 24, width = 2, format = "d", flag = "0"), " h ",
         formatC(t %/% 60 %% 60, width = 2, format = "d", flag = "0"), " min ",
         formatC(t %% 60, width = 2, format = "d", flag = "0"), "s"
               
        
  )
}


