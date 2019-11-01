library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)

# User Interface ----------------------------------------------------------

ui <- navbarPage(theme = "semo_mods.css",
                 windowTitle = "BI 063: Resource partitioning",
                 title=div(img(src="semo_logo.png", height="70px"), 
                           "Bumble bee resource partitioning"),
  #strong("Bumble bee resource partitioning"),

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
                proboscis length on corolla length."
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

               actionButton(inputId = "anova_update", label = "Analyze")
               ),
             mainPanel(
               plotOutput(outputId = "histogram_plot"),
               tags$hr(),
               h4("Summary statistics"),
               DTOutput('tbl', width = "500px"),
#               tableOutput(outputId = "mean_table"),
               tags$hr(),
               h4("ANOVA summary"),
               tableOutput(outputId = "anova_table"),
               tags$hr()
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
          value = FALSE
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
 
