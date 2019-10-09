## Example code from Rstudio on reading an external CSV file.
## https://shiny.rstudio.com/reference/shiny/0.14/fileInput.html

library(shiny)
library(tidyverse)
library(broom)

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
        tableOutput("lm"),
        plotOutput("regression_plot")
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
    
    output$regression_plot <- renderPlot({
      
      model <- summary(lm(corolla ~ proboscis, data = df())) %>% glance()
      r2 <- round(model$r.squared, 2)
      
      cor_r <- round(cor(df()$corolla, df()$proboscis), 2)
      
      p1 <- ggplot(data = df(), aes(x = proboscis, y = corolla)) + 
        geom_point() +
        labs(title = paste("R^2 =", r2,". Correlation =", cor_r),
             x = "Proboscis Length (mm)",
             y = "Corolla Length (mm)") +
        geom_smooth(method = "lm",  se = FALSE)
      print(p1)
      }
    )
    
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
