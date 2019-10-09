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
          "Upload the csv file with the proboscis and corolla length data.
          Your file must be in csv format to upload."
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
        plotOutput(outputId = "plot"),
        tags$hr(),
        h4("Analysis summary"),
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
      updateSelectInput(session, 
                        "var1", 
                        choices = names(contentsrea()),
                        selected = names(contentsrea()[1]))
      updateSelectInput(session, 
                        "var2", 
                        choices = names(contentsrea()),
                        selected = names(contentsrea()[2]))
    })
    
    observeEvent(input$update, {
      if(!is.null(input$datafile)){
        
        df <- read.csv(input$datafile$datapath, header = TRUE, sep = ",")
        var1 <- df[, which(colnames(df) == input$var1)]
        var2 <- df[, which(colnames(df) == input$var2)]
        dat <- data.frame(var1 = var1, var2 = var2)
        corResult <- cor.test(x = var1, y = var2)
        
        bee_lm <- lm(var2 ~ var1, data = dat)
        bee_sum <- summary(bee_lm)
        
        r2 <- round(bee_sum$r.squared, 2)
        intercept <- round(bee_sum$coefficients[[1]], 2)
        slope <- round(bee_sum$coefficients[[2]], 2)
        if (intercept >= 0) {
          sign <- " + "} else {
            sign <-  " - "
            intercept <-abs(intercept)
        }
        
        regression_formula = paste0("Y = ", slope, "X", sign, intercept)
        
        # Table
        summary_df <- data.frame(bee_sum$coefficients)
        
        rownames(summary_df) <- c("Intercept", "Proboscis length")
        colnames(summary_df) <- c("Estimate", "Std. Error", "t Value", "Probability")
        output$table <- renderTable(summary_df, rownames = TRUE)
        
        # Plot
        fit <- lm(var2 ~ var1, data = dat)
        dat$predicted <- predict(fit)
        ggObj <- ggplot(data = dat, aes(x = var1, y = var2)) +
          geom_point(color='gray60', size = 3) +
          labs(x = paste(input$var1, "(mm)"),
               y = paste(input$var2, "(mm)"),
               title = bquote(.(regression_formula) * ".   " ~ R^2 ~ " = " ~ .(r2))) +
#               title = paste(regression_formula, ".", r_two, "= ", r2, ".")) +
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
