## Original Code from 
## https://blog.efpsa.org/2019/04/24/7-easy-steps-to-building-your-own-shiny-app-from-scratch/

library(shiny)

ui <- fluidPage(
  titlePanel("Correlation"),
  sidebarLayout(
    sidebarPanel(
      
      p("This is an application exploring the correlation analysis. Upload some data and inspect your correlation. Your file must be in csv format to upload."),
      
      fileInput(inputId = "datafile", label = "Upload a data file", multiple = FALSE, placeholder = "No file selected", accept = "csv"),
      
      selectInput(inputId = "var1", label = "Variable 1", choices = ""),
      selectInput(inputId = "var2", label = "Variable 2", choices = ""),
      
      p(strong("Plot")),
      checkboxInput(inputId = "regressionLine", label = "Show regression line", value = TRUE),
      checkboxInput(inputId = "residuals", label = "Show residuals", 
                    value = FALSE),
      
      p(strong("Table")),
      checkboxInput(inputId = "conf.int", label = "Display confidence interval", value = FALSE),
      
      actionButton(inputId = "update", label = "Update")
    ),
    mainPanel(
      h3("Result"),
      plotOutput(outputId = "plot"),
      tableOutput(outputId = "table")
    )
  )
)

server <- function(input, output, session) {
  contentsrea <- reactive({
    inFile <- input$datafile
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath)
  })
  observe({
    updateSelectInput(session, "var1", choices = names(contentsrea()))
    updateSelectInput(session, "var2", choices = names(contentsrea()))
  })
  
  observeEvent(input$update, {
    if(!is.null(input$datafile)){
      
      df <- read.csv(input$datafile$datapath, header = TRUE, sep = ",")
      var1 <- df[, which(colnames(df) == input$var1)]
      var2 <- df[, which(colnames(df) == input$var2)]
      dat <- data.frame(var1 = var1, var2 = var2)
      corResult <- cor.test(x = var1, y = var2)
      
      # Table
      df <- data.frame("Variable 1" = input$var1, "Variable 2" = input$var2, "Correlation" = corResult$estimate, "p-value" = corResult$p.value, check.names = FALSE)
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
        geom_point(color='black', size = 3, stroke = 2, fill = "gray60", shape = 21) +
        xlab(input$var1) +
        ylab(input$var2) +
        theme_classic() +
        theme(axis.line = element_line(size = 1.5),
              axis.title = element_text(size = 17),
              axis.text = element_text(size = 17,
                                       color = "black"),
              axis.ticks = element_line(size = 1.5),
              axis.ticks.length=unit(.25, "cm"))
      
      if(input$regressionLine){
        ggObj <- ggObj + geom_smooth(method = "lm", se = FALSE, color = "darkred", size = 1.5)
      }
      
      if(input$residuals){
        ggObj <- ggObj + geom_segment(aes(xend = var1, yend = predicted), size = 1)
      }
      output$plot <- renderPlot(ggObj)
    }
  })
}

shinyApp(ui, server)

