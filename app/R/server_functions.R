
validate_filters<- function(input) {
  # P-value filters should be numbers between 0 and 1
  p_filters <- c("p_limit", "var_p_limit")
  for (p_filter in p_filters) {
    if (input[[p_filter]] != ""){
      p_num <- suppressWarnings(as.numeric(input[[p_filter]]))
      valid_p <- !is.na(p_num) && p_num >= 0 && p_num <= 1
      feedbackDanger(p_filter, !valid_p, "P-value limit should be between 0 an 1")
      req(valid_p)
    }
  }
  # These filters should be numeric
  numerics <- c("n_limit",
                names(input)[grepl("_min$", names(input)) | grepl("_max$", names(input))])
  for (num in numerics) {
    if (!is.null(input[[num]]) && input[[num]] != "") {
      valid_num <- suppressWarnings(!is.na(as.numeric(input[[num]])))
      feedbackDanger(num, !valid_num, "Input should be numeric")
      req(valid_num)
    }
  }
  
  TRUE
}

# Filter a data frame by keywords on columns
# keywords that start with a '-' are exclusions from the search
# keywords that end with a '*' are wildcards
filter_by_keyword <- function(dframe, cols, keywords){
  
  # Split at ',' and remove leading and trailing whitespace
  keywords <- keywords  %>% strsplit(split=",") %>% unlist() %>% trimws()
  
  # Separate exclusions and iclusions
  inclusions <- keywords[!grepl("^-",keywords)]
  exclusions <- keywords[grepl("^-",keywords)] %>% gsub("^-","",.)
  
  if(length(inclusions)){
    # Limit associations to those matching inlcusions
    keep <- rep(FALSE, nrow(dframe))
    
    # First find all the values matching wildcards
    wildcards <- inclusions[grepl("\\*$", inclusions)]
    if(length(wildcards)){
      for(wc in wildcards){
        wc <- paste0("^", gsub("\\*", ".\\*", wc))
        for (col in cols) {
          keep <- keep | grepl(wc, dframe[, col])
        }
      }
    }
    # Then add non-wildcard variables
    inclusions <- inclusions[!grepl("\\*$",inclusions)]
    if(length(inclusions)){
      for (col in cols) {
        keep <- keep | dframe[, col] %in% inclusions
      }
    }
  } else {
    # Keep all rows before exclusions
    keep <- rep(TRUE, nrow(dframe))
  }
  
  # Exclude by keywords marked with '-'
  if(length(exclusions)){
    # Exclude variables marked with '*' wildcard
    wildcards <- exclusions[grepl("\\*$",exclusions)]
    for(wc in wildcards){
      wc <- paste0("^", gsub("\\*", ".\\*", wc))
      # Remove all rows where any of the cols matches wildcard
      for (col in cols) {
        keep <- keep & !grepl(wc, dframe[, col])
      }
    }
    # Exclude non-wildcard variables
    exclusions <- exclusions[!grepl("\\*$", exclusions)]
    # Remove all rows where any of the cols matches any of the exclusions
    for (col in cols) {
      keep <- keep & !dframe[, col] %in% exclusions
    }
  }
  
  keep
}

filter_min_max <- function(dframe, col, min, max) {
  keep <- NULL
  if (min != "" || max != ""){
    if (min == ""){
      keep <- dframe[, col] < as.numeric(max)
    }
    else if (max == ""){
      keep <- dframe[, col] > as.numeric(min)
    }
    else{
      keep <- dframe[, col] < as.numeric(max) & dframe[, col] > as.numeric(min)
    }
  }
  keep
}

varfilter_helper <- function(dframe, tmp_keep, varnum) {
  if(varnum == 2){
    vars_accepted <- c(dframe$Variable1[tmp_keep] , dframe$Variable2[tmp_keep]) %>% unique()
    keep <- dframe$Variable1 %in% vars_accepted | dframe$Variable2 %in% vars_accepted
  }
  else{
    vars_accepted <- unique(dframe$Variable1[tmp_keep])
    keep <- dframe$Variable1 %in% vars_accepted
  }
  keep
}

# Filter variables by p-value, keep all variables that
# have at least one association meeting criterion
varfilter_p <- function(dframe, p_limit, varnum, fdr = FALSE){
  if (fdr){
    p_keep <- dframe$P_FDR < as.numeric(p_limit)
  }
  else{
    p_keep <- dframe$P < as.numeric(p_limit)
  }
  keep <- varfilter_helper(dframe, p_keep, varnum)
}

# Filter variables by effect, keep all variables that
# have at least one association meeting criterion
varfilter_eff <- function(dframe, eff_min, eff_max, varnum){
  eff_keep <- filter_min_max(dframe, col = "Effect", min = eff_min, max = eff_max)
  keep <- varfilter_helper(dframe, eff_keep, varnum)
  keep
}