library(tidyverse)
library(shiny)
library(lubridate)

ui <- fluidPage(

    titlePanel("Sales report for Square data, CSV inputs"),

    mainPanel(

        fileInput(inputId="input_csv", 
            label="Give me a square space export CSV(s)",
            multiple=T,
            accept=c("csv","text/csv"),
            placeholder="placeholder"
            ),

        textOutput("files_read",inline=F),

        downloadButton("report", "Generate report"),

        NULL)
    )

server <- function(input, output) {

    output$files_read <- reactive({
        name_vector <- input$input_csv$name
        if (is.null(name_vector)) {
            return("")
        }
        name_list <- paste(input$input_csv$name,collapse=", ")
        return(paste0("I'm reading in these files : ",name_list))
        })

    output$report <- downloadHandler(
        filename=paste0("report_",
                gsub("\\s","_",Sys.time()),".html"),
        content = function(filename) {
            temp_report_path <- file.path( tempdir(), "report.Rmd" )
            file.copy( "report.Rmd", temp_report_path, overwrite=T )
            params <- list(filepaths=list.files(input$input_csv$name,
                    full.names=T)
                )
            rmarkdown::render(temp_report_path,
                output_file=filename,
                params=params,
                envir=new.env(parent=globalenv())
                )
        })

}

shinyApp(ui = ui, server = server)
