## Example code from Rstudio on reading an external CSV file.
## https://shiny.rstudio.com/reference/shiny/0.14/fileInput.html

library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)

#library(broom)

# Globals -----------------------------------------------------------------

# Used for labeling plots
bombus_species_alphabetical <- c("B. appositus", 
                                 "B. bifarius", 
                                 "B. frigidus", 
                                 "B. kirbiellus", 
                                 "B. sylvicola")

# Warning to upload file before analysis can begin.
upload_warning <- function(){
  showModal(modalDialog(
    title = "Error",
    "Upload a csv file first!"
  ))
}

# User Interface ----------------------------------------------------------


ui <- navbarPage(
  "Bumble bee partitioning",

# Instruction tab ---------------------------------------------------------

  tabPanel("Instructions",
           mainPanel(
             p(
               "Use this site to help you analyze the",
               em("Bombus"), "bumble bee data for resource 
               partitioning."
             ),
             
             p(
               "Choose the Histogram and ANOVA tab above to visualize and 
              statistically analyze the proboscis lengths for the five 
               species of", em("Bombus.")
             ),
             
             p(
               "Choose the Linear Regression tab above to perform a regression analysis of
               corolla length on proboscis length."
             ),
             tags$hr(),
             
             img(src='bombus_bifarius.jpg', align = "middle"),
             tags$br(),
             p(em("Bombus bifarius"), "foraging for nectar. It's extended proboscis 
               is visible in the lower center of the image.",
               tags$br(),
               tags$a(href="https://commons.wikimedia.org/wiki/File:Bombus_bifarius_P1530268a.jpg",
                    "Photo credit: Robert Webster, Wikimedia Commons, CC-SA 4.0"))
             )),

# Histogram and ANOVA tab -------------------------------------------------

  tabPanel("Histograms and ANOVA",
           sidebarLayout(
             sidebarPanel(
               p(
                 "Upload the csv file with the proboscis length data for the five",
                 em("Bombus"), "species. Your file must be in csv format to upload."
               ),
               
               fileInput(
                 inputId = "anova_file",
                 label = "Upload a data file",
                 multiple = FALSE,
                 placeholder = "No file selected",
                 accept = "csv"
               ),
               
               sliderInput(
                 inputId = "alpha_slider",
                 label = "Bar Transparency",
                 min = 0.2,
                 max = 1.0,
                 value = 0.7,
                 step = 0.1
               ),

               # p(strong("Plot")),
               # checkboxInput(
               #   inputId = "unused_regressionLine",
               #   label = "Show regression line",
               #   value = FALSE
               # ),
               # 
               # p(strong("Table")),
               # checkboxInput(
               #   inputId = "unused_conf.int",
               #   label = "Display confidence interval",
               #   value = FALSE
               # ),
               
               actionButton(inputId = "anova_update", label = "Analyze")
               ),
             mainPanel(
               plotOutput(outputId = "histogram_plot"),
               tags$hr(),
               h4("Summary statistics"),
               tableOutput(outputId = "mean_table"),
               tags$hr(),
               h4("ANOVA summary"),
               tableOutput(outputId = "anova_table")
             )
           )
  ),

# Linear Regression Tab Panel ---------------------------------------------

  tabPanel(
    "Linear Regression",
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
        
        p(strong(textOutput(outputId = "warning"))),

        # p(strong("Table")),
        # checkboxInput(
        #   inputId = "conf.int",
        #   label = "Display confidence interval",
        #   value = FALSE
        # ),
        
        actionButton(inputId = "update", label = "Analyze")
      ),
      mainPanel(
        plotOutput(outputId = "plot"),
        tags$hr(),
        h4("Analysis summary"),
        tableOutput(outputId = "table")
      )
    )
  )
  # end tabPanel Linear regression
  )# End navbarPage
 

# Server ------------------------------------------------------------------

  server <- function(input, output, session) {
    

# Histogram and ANOVA -----------------------------------------------------

    anova_data <- reactive({
      inFile <- input$anova_file
      if (is.null(inFile))
        return(NULL)
      read.csv(inFile$datapath) 
    })

    observeEvent(input$anova_update, {
      if (!is.null(input$anova_file)) {
        
        df <- read.csv(input$anova_file$datapath, header = TRUE, sep = ",")
        prob_lengths <- df %>% 
          gather(key = species, value = length, factor_key = TRUE) %>% 
          mutate(species = factor(species, 
                                  levels = c("appositus",
                                             "bifarius",
                                             "frigidus",
                                             "kirbiellus",
                                             "sylvicola"),
                                  ordered = TRUE,
                                  labels = bombus_species_alphabetical))
        summary_stats <- prob_lengths %>% 
          group_by(species) %>% 
          summarize(mean = mean(length),
                    std_dev = sd(length),
                    std_err = sd(length)/sqrt(n()),
                    N = n())

        # Plot
        anova_ggObj <- ggplot(data = prob_lengths) +
          geom_histogram(aes(x = length,
                             fill = species),
                         alpha = input$alpha_slider,
                         position = "identity",
                         binwidth = 0.2,
                         color = "gray50") +
          labs(x = "Proboscis length (mm)",
               y = "Number of individuals",
               fill = "Species") +
          theme_bw() +
          scale_x_continuous(limits = c(6, 14),
                           breaks = seq(6, 14, 1)) +
          scale_y_continuous(limits = c(0, 14),
                             breaks = seq(0, 14, 2)) +
          theme(axis.title = element_text(size = 17),
                axis.text = element_text(size = 17)) +
          theme(legend.text = element_text(face = "italic")) +
          theme(panel.grid = element_line(color = "white"))
      
        output$histogram_plot <- renderPlot(anova_ggObj)
        
        # ANOVA
        
        #bee_lm <- lm(length ~ species, data = prob_lengths)
        bee_aov <- aov(length ~ species, data = prob_lengths)
        aov_summary <- summary(bee_aov)
        bee_summary <- do.call(rbind.data.frame, summary(bee_aov))
        rownames(bee_summary) <- c("Species", "Residuals")
        
        
        # Tables
        colnames(summary_stats) <- c("Species", "Mean", "Std. Deviation", "Std. Error", "N")
        output$mean_table <- renderTable(summary_stats)
        
        output$anova_table <- renderTable(
          bee_summary, rownames = TRUE
        )

      } else {
        upload_warning()
      }
    })

# Linear Regression -------------------------------------------------------

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
          theme_bw() +
          scale_x_continuous(limits = c(5, 13),
                             breaks = seq(5, 13, 1)) +
          scale_y_continuous(limits = c(3, 13),
                             breaks = seq(3, 13, 1)) +
          theme(axis.title = element_text(size = 17),
                axis.text = element_text(size = 17)) +
          theme(panel.grid = element_blank())
        
        if(input$regressionLine){
          ggObj <- ggObj + 
            geom_smooth(method = "lm",
                        se = FALSE, 
                        color = "darkred", 
                        size = 1.5)
        }
        
        output$plot <- renderPlot(ggObj)
      } else {
        #showNotification("Upload a file first!", type = "error")
        upload_warning()
      }
    })
  }

shinyApp(ui, server)
