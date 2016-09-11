# Returns the assocsiations table with variables matching the datasets defined by dataset labels and tags
# Returns NULL if no datasets were found
# Returns -1 if there are multiple different numbers of variables or effect types
get_associations_by_ds <- function(conn,ds_label){
  ds_tbl <- conn %>% tbl("datasets") %>% filter(label == ds_label)
  
  ds_tbl_df <- collect(ds_tbl)
  
  if (dim(ds_tbl_df)[1] == 0 ){
    return (list(associations_tbl = data.frame(),varnum = -1, effect_type = "None"))
  }
  
  varnum <- ds_tbl_df$varnum %>% unique()
  effect_type <-ds_tbl_df$effect_type %>% unique()
  
  
  
  assocs_tbl <- conn %>% tbl("associations") %>% semi_join(ds_tbl, by = c("dataset_id" = "id"))
  
  return (list(associations_tbl =assocs_tbl,varnum = varnum,effect_type = effect_type))
  
}

get_datasets <- function(conn){
  ds_tbl <- conn %>% tbl("datasets") %>% collect()
}
# Joins the variables to the associations table
# Returns COLLECTED local data frame
join_variables <- function(conn,assocs_tbl,varnum){
  
  # Searches the variables connected to the associations and joins them to the assocs_tbl
  assoc_to_var_tbl <- conn %>% tbl("associationtovariable") %>% 
    semi_join(assocs_tbl, by = c("association_id" = "id"))
  var_tbl <- conn %>% tbl("variables") %>% 
    inner_join(assoc_to_var_tbl, by = c("id" = "variable_id"))
  assocs_tbl <- left_join(assocs_tbl,var_tbl,by = c("id" = "association_id"))
  
  # Removes unnecessary columns
  if (varnum == 1){
    assocs_tbl <- assocs_tbl %>%
      select(-id,-id.x,-id.y,-dataset_id,-variable_id,-association_id) %>%
      collect()
  }
  
  # Removes unnecessary columns and combines the rows of same association
  # (Before this the table had two rows with the same association information, but only one variable each)
  if (varnum == 2){
    assocs_tbl <- assocs_tbl %>%
      select(-id,-id.x,-id.y,-dataset_id,-variable_id) %>%
      group_by(association_id) %>%
      collect() %>%
      mutate(var_labels = paste(label[1],label[2],sep = ";"),var_descriptions = paste(description[1],description[2],sep = ";")) %>%
      ungroup() %>%
      select(-association_id,-description,-label) %>%
      distinct() %>% 
      separate(var_labels, c("var_label1","var_label2"),sep = ";") %>%
      separate(var_descriptions, c("var_description1","var_description2"), sep = ";")
  }
  
  return (assocs_tbl)
}

filter_vars <- function(assocs_tbl,var_labels,varnum){
  var_labels <- var_labels  %>% strsplit(split=",") %>% unlist
  if (length(var_labels) == 1){
    if (varnum == 1){
      assocs_tbl <- assocs_tbl %>% filter(label == var_labels)
    }
    if (varnum == 2){
      assocs_tbl <- assocs_tbl %>% filter(var_label1 == var_labels | var_label2 == var_labels)
    }
  }
  else{
    if (varnum == 1){
      assocs_tbl <- assocs_tbl %>% filter(label %in% var_labels)
    }
    if (varnum == 2){
      assocs_tbl <- assocs_tbl %>% filter(var_label1 %in% var_labels | var_label2 %in% var_labels)
    }
  }
  assocs_tbl
}

make_pretty <- function(df,varnum){
  if (varnum == 1){
    df <- df %>% select(label,effect_l95,effect_u95,effect,n,p,p_fdr,description) %>%
      rename(Variable = label, Effect_CIL95 = effect_l95, Effect_CIU95 = effect_u95, Effect = effect,
             N = n, P = p, P_FDR = p_fdr,Description = description)
  }
  if (varnum == 2){
    df <- df %>% select(var_label1,var_label2,effect_l95,effect_u95,effect,n,
                        p,p_fdr,var_description1,var_description2) %>%
      rename(Variable1 = var_label1, Variable2 = var_label2, Effect_CIL95 = effect_l95, Effect_CIU95 = effect_u95,
             Effect = effect, N = n, P = p, P_FDR = p_fdr,Description1 = var_description1, Description2 = var_description2)
  }
  df
}

# Filters the pre-collected associations table, shows only the chosen variables
# Returns COLLECTED local data frames

# NOTE: does not have variable labels!! FIX!

# filter_by_var <- function(conn,assocs_tbl,var_labels,varnum){
#   var_labels <- var_labels  %>% strsplit(split=",") %>% unlist
#   if (length(var_labels) == 1){
#     var_tbl <- conn %>% tbl("variables") %>% filter(label == var_labels | description == var_labels)
#   }
#   else{
#     var_tbl <- conn %>% tbl("variables") %>% filter(label %in% var_labels | description %in% var_labels)
#   }
#   
#   if (varnum == 1){
#     assoc_to_var_tbl <- conn %>% tbl("associationtovariable") %>% inner_join(var_tbl,by = c("variable_id" = "id"))
#     assocs_tbl <- inner_join(assocs_tbl,assoc_to_var_tbl,by = c("id" = "association_id"))
#   }
#   if (varnum == 2){
#     assoc_to_var_tbl <- conn %>% tbl("associationtovariable") %>% semi_join(var_tbl,by = c("variable_id" = "id"))
#     assocs_tbl <- semi_join(assocs_tbl,assoc_to_var_tbl,by = c("id" = "association_id")) %>%
#       join_variables(conn,assocs_tbl,varnum)
#   }
#   
#   return(assocs_tbl)
# }