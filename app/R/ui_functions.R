



column_filter <- function(x, col, input) {
  out <- NULL
  if(class(x) == "numeric"){
    out <- tagList(out,
                   strong(col),
                   fluidRow(
                     column(5,
                            textInput(paste0(col, "_min"), label = "min",
                                      value = isolate(input[[paste0(col, "_min")]]))
                     ),
                     column(5,
                            textInput(paste0(col, "_max"), label = "max",
                                      value = isolate(input[[paste0(col, "_max")]]))
                     )
                   ))
    
  }
  if(class(x) == "character"){
    out <- tagList(out,
                   strong(col),
                   textInput(paste0(col, "_label"),
                             label = "Keywords, comma separated",
                             value = isolate(input[[paste0(col,"_label")]])))
  }
  out
}