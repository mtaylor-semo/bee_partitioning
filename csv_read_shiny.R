## Example code from Rstudio on reading an external CSV file.
## https://shiny.rstudio.com/reference/shiny/0.14/fileInput.html

library(shiny)
#library(ggplot)

## Only run examples in interactive R sessions
  ui <- navbarPage(
    "Bumble bee partitioning",
    tabPanel("Upload file",
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        tags$hr(),
        checkboxInput("header", "Header", TRUE)
      ),
      mainPanel(
        tableOutput("contents")
        
        #tableOutput("model")
      )
    )
  ),  # end tabPanel Upload
  tabPanel(
    "ANOVA",
    sidebarLayout(
      sidebarPanel(
        helpText("ANOVA goes here"),
        uiOutput("anova_columns_menu")
      ),
      mainPanel(
        helpText("ANOVA goes here"),
        tableOutput("anova")
      )
    )
  ),
  tabPanel(
    "Linear Regression",
    sidebarLayout(
      sidebarPanel(
        helpText("Linear Regression goes here"),
        uiOutput(("lm_columns_menu"))
      ),
      mainPanel(
        tableOutput("lm")
      )
    )
  )
  
  
  )# End navbarPage
  
  server <- function(input, output) {
    df = reactive({
      req(input$file1)
      read.csv(file = input$file1$datapath)
    })
    
    # Get column names for UI menu.
    output$anova_columns_menu <- renderUI({
      selectInput("cols", "Column Names", names(df()))
    })

    output$lm_columns_menu <- renderUI({
      selectInput("cols", "Column Names", names(df()))
    })
    
    output$contents <- renderTable({
      df()
    })
    
    output$lm <- renderTable({
      req(df())
      bee_lm <- lm(corolla ~ proboscis, data = df())
      return(
        data.frame(
          summary(bee_lm)$coefficient)
      )
    },
    rownames = TRUE)
    
    output$anova <- renderTable({
      req(df())
      bee_lma <- lm(corolla ~ proboscis, data = df())
      bee_anova <- aov(bee_lma)
      return(
        data.frame(
          summary(bee_anova)[[1]]))
    },
    rownames = TRUE)
  }
  
  shinyApp(ui, server)
