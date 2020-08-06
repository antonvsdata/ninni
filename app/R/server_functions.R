
make_ds_info_table <- function(asso_list) {
  string <- c("Number of datasets","Effect type(s)", "Number of associations:",
              "Number of unique variables:","P-value < 0.05","P-value (adjusted) < 0.05",
              "P-value range:","Effect range:")
  values <- c(nrow(asso_list$datasets),
              asso_list$datasets$effect_type %>% unique() %>% paste(collapse=","),
              nrow(asso_list$dframe))
  if (asso_list$varnum == 2){
    values <- c(values, c(asso_list$dframe$Variable1, asso_list$dframe$Variable2) %>%
                  unique() %>% length())
  }
  else{
    values <- c(values, asso_list$dframe$Variable1 %>%
                  unique() %>% length())
  }
  values <- c(values, asso_list$dframe %>% filter(P < 0.05) %>% nrow(),
              asso_list$dframe %>% filter(P_adj < 0.05) %>% nrow(),
              paste((asso_list$dframe$P) %>% min() %>% signif(digits = 3), "...",
                    (asso_list$dframe$P) %>% max() %>% signif(digits = 3)),
              paste((asso_list$dframe$Effect) %>% min() %>% signif(digits = 3), "...",
                    (asso_list$dframe$Effect) %>% max() %>% signif(digits = 3)))
  data.frame(string, values)
}


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

filter_associations_dframe <- function(asso_list, input) {
  dframe <- as.data.frame(asso_list$dframe)
  # List of all filters
  # Combined to one logical in the end
  keeps <- list()
  
  #Variable filters
  # keep variable with at least one association that satisfies constraints
  # P-value <
  if(input$toggle_variable_filters){
    if (input$var_p_limit != ""){
      keeps$var_p <- varfilter_p(dframe, input$var_p_limit,
                                 asso_list$varnum, input$var_p_limit_adj)
    }
    
    # Effect: min max
    if ((input$var_eff_min != "" | input$var_eff_max != "")){
      keeps$var_eff <- varfilter_eff(dframe, eff_min = input$var_eff_min,
                                     eff_max = input$var_eff_max,
                                     varnum = asso_list$varnum)
    }
  }
  
  # Association filters:
  if(input$toggle_standard_filters){
    # Variable
    # Keywords, comma separated
    if (input$var_labels != ""){
      if(asso_list$varnum == 2){
        cols <- c("Variable1", "Variable2")
      }
      else{
        cols <- "Variable1"
      }
      keeps$variables <- filter_by_keyword(dframe, cols, input$var_labels)
    }
    # Description
    # Keywords, comma separated
    if (input$description_labels != ""){
      if(asso_list$varnum == 2){
        cols <- c("Description1", "Description2")
      }
      else{
        cols <- "Description1"
      }
      keeps$description <- filter_by_keyword(dframe, cols, input$description_labels)
    }
    # P-value <
    if(input$p_limit != ""){
      if (input$p_limit_adj){
        keeps$assoc_p <- dframe$P_adj < as.numeric(input$p_limit)
      }
      else{
        keeps$assoc_p <- dframe$P < as.numeric(input$p_limit)
      }
    }
    # Minimum N
    if (input$n_limit != ""){
      keeps$assoc_n <- dframe$N >= as.numeric(input$n_limit)
    }
    # Effect size: min max
    if (input$eff_min != "" || input$eff_max != ""){
      keeps$assoc_eff <- filter_min_max(dframe, "Effect",
                                        input$eff_min, input$eff_max)
    }
  }
  
  # Filters for extra variables
  if(input$toggle_extra_filters){
    if(asso_list$varnum == 2){
      col_limit <- 12
    }
    else{ # each varnum == 1
      col_limit <- 10
    }
    if(ncol(dframe) > col_limit){
      extra_cols <- colnames(dframe)[seq(col_limit + 1, ncol(dframe))]
      for(col in extra_cols){
        if(class(dframe[, col]) == "numeric" &&
           !is.null(input[[paste0(col, "_min")]]) &&
           !is.null(input[[paste0(col, "_max")]])){
          keeps[[col]] <- filter_min_max(dframe,
                                         col = col,
                                         min = input[[paste0(col, "_min")]],
                                         max = input[[paste0(col, "_max")]])
        }
        else if(class(dframe[, col]) == "character" &&
                !is.null(input[[paste0(col, "_label")]])){
          keywords <- input[[paste0(col, "_label")]]
          if(keywords != ""){
            keeps[[col]] <- filter_by_keyword(dframe, col, keywords)
          }
        }
      }
    }
    
  }
  
  if (length(keeps)) {
    keep_master <- reduce(keeps, `&`)
    asso_list$dframe <- dframe[keep_master, ]
  }
  
  return(asso_list)
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
varfilter_p <- function(dframe, p_limit, varnum, p_adj = FALSE){
  if (p_adj){
    p_keep <- dframe$P_adj < as.numeric(p_limit)
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