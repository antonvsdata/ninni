

# Returns the assocsiations table with variables matching the datasets defined by dataset labels and tags
# Returns NULL if no datasets were found
# Returns -1 if there are multiple different numbers of variables or effect types
get_associations_by_ds <- function(conn,ds_labels,ds_tags){
  ds_tbl <- conn %>% tbl("datasets")
  if (ds_labels != ""){
    ds_labels <- ds_labels %>% strsplit(split=",") %>% unlist
    if (length(ds_labels) == 1){
      ds_tbl <- ds_tbl %>% filter(label == ds_labels)
    }
    else{
      ds_tbl <- ds_tbl %>% filter(label %in% ds_labels)
    }
  }
  
  if (ds_tags!= ""){
    ds_tags <- ds_tags %>% strsplit(split=",") %>% unlist
    if (length(ds_tags) == 1){
      meta_tbl <- conn %>% tbl("datasetmetadata") %>% filter(label == ds_tags | description == ds_tags)
      ds_to_meta_tbl <- conn %>% tbl("datasettometadata") %>% semi_join(meta_tbl,by = c("datasetmetadata_id" = "id"))
      ds_tbl <- semi_join(ds_tbl,ds_to_meta_tbl,by = c("id" = "dataset_id"))
    }
    else{
      meta_tbl <- conn %>% tbl("datasetmetadata") %>% filter(label %in% ds_tags | description %in% ds_tags)
      ds_to_meta_tbl <- conn %>% tbl("datasettometadata") %>% semi_join(meta_tbl,by = c("datasetmetadata_id" = "id"))
      ds_tbl <- semi_join(ds_tbl,ds_to_meta_tbl,by = c("id" = "dataset_id"))
    }
  }
  
  ds_tbl_df <- collect(ds_tbl)
  varnum <- ds_tbl_df$varnum %>% unique()
  effect_type <-ds_tbl_df$effect_type %>% unique()
  
  if (dim(ds_tbl_df)[1] == 0 | length(varnum) > 1 | length(effect_type) > 1){
    return (list(ds_tbl = ds_tbl_df, associations_tbl = data.frame(),varnum = -1, effect_type = "Differs"))
  }
  
  assocs_tbl <- conn %>% tbl("associations") %>% semi_join(ds_tbl, by = c("dataset_id" = "id"))
  
  #assocs_tbl <- join_variables(conn,assocs_tbl,varnum)
  
  return (list(ds_tbl = ds_tbl_df,associations_tbl =assocs_tbl,varnum = varnum,effect_type = effect_type))
  
}
# Joins the variables to the associations table
# Returns COLLECTED local data frame
join_variables <- function(conn,assocs_tbl,varnum){
  assoc_to_var_tbl <- conn %>% tbl("associationtovariable") %>% 
    semi_join(assocs_tbl, by = c("association_id" = "id"))
  var_tbl <- conn %>% tbl("variables") %>% 
    inner_join(assoc_to_var_tbl, by = c("id" = "variable_id"))
  assocs_tbl <- left_join(assocs_tbl,var_tbl,by = c("id" = "association_id"))
  
  if (varnum == 1){
    assocs_tbl <- assocs_tbl %>%
      select(-id,-id.x,-id.y,-dataset_id,-variable_id,-association_id) %>%
      collect()
  }
  
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
# Filters the associations table, shows only the chosen variables
# Returns COLLECTED local data frames
filter_by_var <- function(conn,assocs_tbl,var_labels){
  var_labels <- input$var_label  %>% strsplit(split=",") %>% unlist
  if (length(var_labels) == 1){
    var_tbl <- conn %>% tbl("variables") %>% filter(label == var_labels | description == var_labels)
  }
  else{
    var_tbl <- conn %>% tbl("variables") %>% filter(label %in% var_labels | description %in% var_labels)
  }
  assoc_to_var_tbl <- conn %>% tbl("associationtovariable") %>% semi_join(meta_tbl,by = c("variable_id" = "id"))
  assocs_tbl <- semi_join(assocs_tbl,assoc_to_var_tbl,by = c("id" = "association_id")) %>% collect()
  
  return(assocs_tbl)
}

