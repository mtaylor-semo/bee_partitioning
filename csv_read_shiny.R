## Example code from Rstudio on reading an external CSV file.
## https://shiny.rstudio.com/reference/shiny/0.14/fileInput.html

library(shiny)
#library(ggplot)

## Only run examples in interactive R sessions
  ui <- fluidPage(
    titlePanel("Bumble bee partitioning"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        tags$hr(),
        checkboxInput("header", "Header", TRUE),
        uiOutput("columns_menu")
      ),
      mainPanel(
        tableOutput("contents"),
        
        tableOutput("model")
      )
    )
  )
  
  server <- function(input, output) {
    df = reactive({
      req(input$file1)
      read.csv(file = input$file1$datapath)
    })
    
    # Get column names for UI menu.
    output$columns_menu <- renderUI({
      selectInput("cols", "Column Names", names(df()))
    })
    
    output$contents <- renderTable({
      df()
    })
    
    output$model <- renderTable({
      req(df())
      bee_lm <- lm(corolla ~ proboscis, data = df())
      return(data.frame(summary(bee_lm)$coefficient))
    },
    rownames = TRUE)
  }
  
  shinyApp(ui, server)
