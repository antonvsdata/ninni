
static_downloader <- function(file_name, plotter, file_format, width, height, large) {
  
  downloadHandler(
    filename = function(){
      paste0(file_name, ".", file_format)
    },
    
    content = function(file){
      p <- plotter()
      
      if (file_format == "png") {
        png(file, width = width, height = height)
        print(p)
        dev.off()
      } else {
        if(large){
          scale <- 1.5
        } else{
          scale <- 1
        }
        ggsave(file, p, width = width/300, height = height/300, dpi = 300, units = "in", scale = scale)
      }
    }
  )
  
}


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