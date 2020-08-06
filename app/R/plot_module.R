
downloadUI <- function(id, large) {
  
  ns <- NS(id)
  
  out <- tagList(
    fluidRow(
      column(1,
             downloadButton(ns("download_static"))
      ),
      column(2,
             radioButtons(ns("download_format"), label = NULL,
                          choices = c("png", "pdf")))
    ),
    conditionalPanel('input.download_format == "pdf"', ns = ns,
                     numericInput(ns("pdf_width"), "PDF width in inches",
                                  value = 8, min = 1),
                     numericInput(ns("pdf_height"), "PDF height in inches",
                                  value = 8, min = 1))
  )
  if (!large()) {
    out <- tagList(out,
                   downloadButton(ns("download_plotly"), "Download Interactive"))
  }
  
  
  out
}

downloadServer <- function(id, plotter, include_plotly = TRUE, large = reactive(FALSE),
                           width, height) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    file_name <- paste0("ninni_", gsub("_download", "", id), ".")
    
    output$download_static <-  downloadHandler(
      filename = function(){
        paste0(file_name, input$download_format)
      },
      
      content = function(file) {
        p <- plotter()
        
        if (input$download_format == "png") {
          png(file, width = width(), height = height())
        } else {
          pdf(file, width = input$pdf_width,
              height = input$pdf_height)
        }
        print(p)
        dev.off()
      }
    )
    
    output$download_plotly <- downloadHandler(
      filename = function(){
        paste0(file_name, "html")
      },
      
      content = function(file) {
        if (!large()) {
          p <- ggplotly(plotter(), tooltip = paste0("label", 1:9),
                        width = width(), height = height())
          saveWidget(as_widget(p), file, selfcontained = TRUE,
                     title = "Plot by Ninni")
        } else {
          stop("nope")
        }
        
      }
    )
  })
  
}


plotUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sliderInput(ns("plot_width"), "Plot width",
                min = 200, max = 3000,
                value = 800),
    sliderInput(ns("plot_height"), "Plot height",
                min = 200, max = 3000,
                value = 600),
    uiOutput(ns("plot_out")),
    uiOutput(ns("download_buttons"))
  )
}


plotServer <- function(id, plotter, large, include_plotly = TRUE,
                       msg = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    width <- reactive({
      input$plot_width
    })
    height <- reactive({
      input$plot_height
    })
    
    output$plot_out <- renderUI({
      if (!is.null(msg())) {
        return(msg())
      }
      
      if (include_plotly && !large()) {
        plotlyOutput(ns("plot_plotly"),
                     height = "100%")
      } else {
        plotOutput(ns("plot_static"),
                   width = width(),
                   height = height())
      }
      
    })
    
    output$plot_plotly <- renderPlotly({
      ggp <- plotter()
      ggplotly(ggp, tooltip = paste0("label", 1:9),
               width = width(), height = height())
    })
    
    output$plot_static <- renderPlot({
      plotter()
    })
    
    output$download_buttons <- renderUI({
      if (is.null(msg())) {
        downloadUI(ns(paste0(id, "_download")), large)
      }
    })
    
    downloadServer(paste0(id, "_download"), plotter, include_plotly, large,
                   width, height)
    
  }
  )
}





