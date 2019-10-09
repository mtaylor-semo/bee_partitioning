## Example code from Rstudio on reading an external CSV file.
## https://shiny.rstudio.com/reference/shiny/0.14/fileInput.html

library(shiny)
library(tidyverse)
library(broom)

#library(ggplot)

ui <- navbarPage(
  "Bumble bee partitioning",
  tabPanel("Instructions",
           mainPanel(
             p(
               "Use this site to help you analyze the bumble bee data for resource partitioning."
             ),
             
             p(
               "Choose the Histogram and ANOVA tab to visualize and statistically analyze
               the proboscis lengths for the five species of Bombus."
             ),
             
             p(
               "Choose the Linear Regression tab to perform a regression analysis of
               corolla length on proboscis length."
             )
             )),
  tabPanel(
    "Linear Regression Tab",
    sidebarLayout(
      sidebarPanel(
        p(
          "This is an application exploring the correlation analysis. Upload some data and inspect your correlation. Your file must be in csv format to upload."
        ),
        
        fileInput(
          inputId = "datafile",
          label = "Upload a data file",
          multiple = FALSE,
          placeholder = "No file selected",
          accept = "csv"
        ),
        
        selectInput(
          inputId = "var1",
          label = "Predictor variable (x-axis)",
          choices = ""
        ),
        selectInput(
          inputId = "var2",
          label = "Response variable (y-axis)",
          choices = ""
        ),
        
        p(strong("Plot")),
        checkboxInput(
          inputId = "regressionLine",
          label = "Show regression line",
          value = TRUE
        ),

        p(strong("Table")),
        checkboxInput(
          inputId = "conf.int",
          label = "Display confidence interval",
          value = FALSE
        ),
        
        actionButton(inputId = "update", label = "Update")
      ),
      mainPanel(
        h3("Result"),
        plotOutput(outputId = "plot"),
        tableOutput(outputId = "table")
      )
    )
  ),
  # end tabPanel Upload
  tabPanel("ANOVA",
           sidebarLayout(
             sidebarPanel(helpText("ANOVA goes here"),
                          uiOutput("anova_columns_menu")),
             mainPanel(helpText("ANOVA goes here"),
                       tableOutput("anova"))
           ))
  
  
  )# End navbarPage
  
  server <- function(input, output, session) {
    contentsrea <- reactive({
      inFile <- input$datafile
      if (is.null(inFile))
        return(NULL)
      read.csv(inFile$datapath)
    })
    observe({
      updateSelectInput(session, "var1", choices = names(contentsrea()))
      updateSelectInput(session, "var2", choices = names(contentsrea())[2])
    })
    
    observeEvent(input$update, {
      if(!is.null(input$datafile)){
        
        df <- read.csv(input$datafile$datapath, header = TRUE, sep = ",")
        var1 <- df[, which(colnames(df) == input$var1)]
        var2 <- df[, which(colnames(df) == input$var2)]
        dat <- data.frame(var1 = var1, var2 = var2)
        corResult <- cor.test(x = var1, y = var2)
        
        # Table
        df <- data.frame("Predictor" = input$var1, 
                         "Response" = input$var2, "Correlation" = corResult$estimate, "p-value" = corResult$p.value, check.names = FALSE)
        if(input$conf.int){
          lowerCI <- corResult$conf.int[1]
          upperCI <- corResult$conf.int[2]
          df <- cbind(df, lowerCI, upperCI)
        }
        rownames(df) <- "Correlation Result"
        output$table <- renderTable(df, rownames = TRUE)
        
        # Plot
        fit <- lm(var2 ~ var1, data = dat)
        dat$predicted <- predict(fit)
        ggObj <- ggplot(data = dat, aes(x = var1, y = var2)) +
          geom_point(color='gray60', size = 3) +
          xlab(paste(input$var1, "(mm)")) +
          ylab(paste(input$var2, "(mm)")) +
          theme_bw() +
          theme(axis.title = element_text(size = 17),
                axis.text = element_text(size = 17))
        
        if(input$regressionLine){
          ggObj <- ggObj + 
            geom_smooth(method = "lm",
                        se = FALSE, 
                        color = "darkred", 
                        size = 1.5)
        }
        
        output$plot <- renderPlot(ggObj)
      }
    })
  }

shinyApp(ui, server)
