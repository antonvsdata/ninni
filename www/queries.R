# Returns the assocsiations table with variables matching the dataset defined by ds_label

get_associations_by_ds <- function(conn,ds_label){
  ds_tbl <- conn %>% tbl("datasets") %>% filter(label == ds_label)
  assocs_tbl <- conn %>% tbl("associations") %>% semi_join(ds_tbl, by = c("dataset_id" = "id"))
  
  ds_tbl_df <- collect(ds_tbl)
  varnum <- ds_tbl_df$varnum
  effect_type <-ds_tbl_df$effect_type
  
  return (list(dframe =assocs_tbl,varnum = varnum,effect_type = effect_type))
}

get_datasets <- function(pool){
  ds_tbl <- src_pool(pool) %>% tbl("datasets") %>% collect()
}

get_metavariables <- function(conn,assocs_tbl){
  
  final_tbl <- NULL
  
  # Get numeric metavariables
  numval_tbl <- conn %>% tbl("numval") %>%
    semi_join(assocs_tbl, by = c("association_id" = "id"))
  metavar_tbl <- conn %>% tbl("metavariables") %>%
    semi_join(numval_tbl, by = c("id" = "metavariable_id")) %>% collect()
  
  # Add metavar as column name and values as column elements to data frame
  if(nrow(collect(numval_tbl))){
    final_tbl <- data.frame()
    for (i in 1:nrow(metavar_tbl)){
      numval_tmp <- collect(numval_tbl) %>% filter(metavariable_id == metavar_tbl$id[i]) %>% select(association_id,value)
      if(nrow(final_tbl) == 0){
        final_tbl <- numval_tmp
        colnames(final_tbl) <- c("association_id",metavar_tbl$label[i])
      }
      else{
        final_tbl <- left_join(final_tbl,numval_tmp,by="association_id")
        colnames(final_tbl)[i+1] <- metavar_tbl$label[i]
      }
    }
  }
  
  #Get character metavariables
  strval_tbl <- conn %>% tbl("strval") %>%
    semi_join(assocs_tbl, by = c("association_id" = "id"))
  metavar_tbl <- conn %>% tbl("metavariables") %>%
    semi_join(strval_tbl, by = c("id" = "metavariable_id")) %>% collect()
  
  # Add metavar as column name and values as column elements to data frame
  if(nrow(collect(numval_tbl))){
    for (i in 1:nrow(metavar_tbl)){
      strval_tmp <- collect(strval_tbl) %>% filter(metavariable_id == metavar_tbl$id[i]) %>% select(association_id,value)
      if(nrow(final_tbl) == 0){
        final_tbl <- strval_tmp
        colnames(final_tbl) <- c("assciation_id",metavar_tbl$label[i])
      }
      else{
        final_tbl <- left_join(final_tbl,strval_tmp,by="association_id")
        colnames(final_tbl)[ncol(final_tbl)] <- metavar_tbl$label[i]
      }
    }
  }
  
  final_tbl
}
# Joins the variables and metavariables to the associations table
# Returns COLLECTED local data frame
join_variables <- function(conn,assocs_tbl,varnum){
  
  assocs_tbl_orig <- assocs_tbl
  
  # Searches the variables connected to the associations and joins them to the assocs_tbl
  assoc_to_var_tbl <- conn %>% tbl("associationtovariable") %>% 
    semi_join(assocs_tbl, by = c("association_id" = "id"))
  var_tbl <- conn %>% tbl("variables") %>% 
    inner_join(assoc_to_var_tbl, by = c("id" = "variable_id"))
  assocs_tbl <- left_join(assocs_tbl,var_tbl,by = c("id" = "association_id"))
  
  # Removes unnecessary columns
  if (varnum == 1){
    assocs_tbl <- assocs_tbl %>%
      select(-id,-id.x,-id.y,-dataset_id,-variable_id) %>%
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
      select(-description,-label) %>%
      distinct() %>% 
      separate(var_labels, c("var_label1","var_label2"),sep = ";") %>%
      separate(var_descriptions, c("var_description1","var_description2"), sep = ";")
  }
  
  #Join metavariables
  metavar_tbl <- get_metavariables(conn,assocs_tbl_orig)
  if(!is.null(metavar_tbl)){
    assocs_tbl <- left_join(assocs_tbl,metavar_tbl,by="association_id")
  }
  assocs_tbl <- select(assocs_tbl,-association_id)
  
  return (assocs_tbl)
}

filter_vars <- function(assocs_tbl,var_labels,varnum){
  var_labels <- var_labels  %>% strsplit(split=",") %>% unlist
    if (varnum == 1){
      assocs_tbl <- assocs_tbl %>% filter(Variable %in% var_labels | Description %in% var_labels)
    }
    if (varnum == 2){
      assocs_tbl <- assocs_tbl %>% filter(Variable1 %in% var_labels | Variable2 %in% var_labels | Description1 %in% var_labels | Description2 %in% var_labels)
    }
  assocs_tbl
}

make_pretty <- function(dframe,varnum){
  if (varnum == 1){
    dframe <- dframe %>% select(label,effect_l95,effect_u95,effect,n,p,p_fdr,description,everything()) %>%
      rename(Variable = label, Effect_CIL95 = effect_l95, Effect_CIU95 = effect_u95, Effect = effect,
             N = n, P = p, P_FDR = p_fdr,Description = description)
  }
  if (varnum == 2){
    dframe <- dframe %>% select(var_label1,var_label2,effect_l95,effect_u95,effect,n,
                        p,p_fdr,var_description1,var_description2,everything()) %>%
      rename(Variable1 = var_label1, Variable2 = var_label2, Effect_CIL95 = effect_l95, Effect_CIU95 = effect_u95,
             Effect = effect, N = n, P = p, P_FDR = p_fdr,Description1 = var_description1, Description2 = var_description2)
  }
  dframe
}

varfilter_p <- function(dframe,p_limit,varnum,fdr = FALSE){
  if (fdr){
    dframe_fltrd <- dframe %>% filter(P_FDR < p_limit)
  }
  else{
    dframe_fltrd <- dframe %>% filter(P < p_limit)
  }
  if (varnum == 1){
    vars_accepted <- unique(dframe_fltrd$Variable)
    dframe <- dframe %>% filter(Variable %in% vars_accepted)
  }
  if (varnum == 2){
    vars_accepted <- c(dframe_fltrd$Variable1,dframe_fltrd$Variable2) %>% unique()
    dframe <- dframe %>% filter(Variable1 %in% vars_accepted | Variable2 %in% vars_accepted)
  }
  dframe
}

varfilter_eff <- function(dframe,eff_min = -Inf,eff_max = Inf,varnum){
  dframe_fltrd <- dframe %>% filter(Effect > eff_min & Effect < eff_max)
  if (varnum == 1){
    vars_accepted <- unique(dframe_fltrd$Variable)
    dframe <- dframe %>% filter(Variable %in% vars_accepted)
  }
  if (varnum == 2){
    vars_accepted <- c(dframe_fltrd$Variable1,dframe_fltrd$Variable2) %>% unique()
    dframe <- dframe %>% filter(Variable1 %in% vars_accepted | Variable2 %in% vars_accepted)
  }
  dframe
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