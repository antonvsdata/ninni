# Read inforamtion for connecting to the database
read_db_info <- function(config_file){
  df <- read.table(config_file)
  
  l <- as.list(as.character(df$V2))
  names(l) <- df$V1
  l
}

# Read SQL statements from a file
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

execute_sql_file <- function(file, con) {
  
  sql <- get_sql(file)
  
  for (query in sql) {
    dbSendQuery(con, statement = query)
  }
  
}


get_missing <- function(x) {
  if (x != "" && !is.na(x) && !file.exists(x)) {
    x
  } else {
    NA_character_
  }
}

# Check that all listed files exist
check_files <- function(datasets) {
  
  
  missing_files <- sapply(c(datasets$DATASET_FILENAME, datasets$VARIABLES_FILENAME), get_missing)
  missing_files <- unique(missing_files[!is.na(missing_files)])
  if (length(missing_files)) {
    stop(paste0("The following files are listed in datasets file but not found:\n",
                paste(missing_files, collapse = "\n")),
         call. = FALSE)
  }
  
}

# Get last ID used in all the tables, or 0 if the table is empty
get_last_ids <- function(con) {
  sapply(dbListTables(con), function(x){
    max(dbReadTable(con, x)$id, 0)
  })
}

# link metavariables to associations
link_metavars <- function(assoc_tmp, assoc, type) {
  val_tmp <- expand.grid(association_id = assoc_tmp$id,
                         metavariable_label = metavariables_tmp$label[metavariables_tmp$type == type],
                         stringsAsFactors = FALSE)
  val_tmp$metavariable_id <- metavariables_tmp[val_tmp$metavariable_label, "id"]
  val_tmp$value <- sapply(seq_len(nrow(val_tmp)), function(j) {
    assoc[as.character(val_tmp$association_id[j]), val_tmp$metavariable_label[j]]
  })
  val_tmp$metavariable_label <- NULL
  val_tmp
}