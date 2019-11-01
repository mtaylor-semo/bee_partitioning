## Example code from Rstudio on reading an external CSV file.
## https://shiny.rstudio.com/reference/shiny/0.14/fileInput.html

library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(DT)

# Globals -----------------------------------------------------------------

# Used for labeling plots
bombus_species_alphabetical <- c("B. appositus", 
                                 "B. bifarius", 
                                 "B. frigidus", 
                                 "B. kirbiellus", 
                                 "B. sylvicola")

# Upload file

upload_file <- function(the_file = NULL) {
  inFile <- the_file
  if (is.null(inFile))
    return(NULL)
  read.csv(inFile$datapath)
}

# Warning to upload file before analysis can begin.
upload_warning <- function(){
  showModal(modalDialog(
    title = "Error",
    "Upload a csv file first!"
  ))
}


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  session$onSessionEnded(stopApp)

# Histogram and ANOVA -----------------------------------------------------

    anova_data <- reactive({
      upload_file(input$anova_file)
    })
    
    observeEvent(input$anova_file, {
      updateActionButton(session, inputId = "anova_update", label = "Analyze")
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
          summarize(mean = round(mean(length),2),
                    std_dev = round(sd(length), 2),
                    std_err = round(sd(length)/sqrt(n()),2),
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
          theme(panel.grid = element_line(color = "white")) +
          guides(fill = guide_legend(override.aes = list(alpha = 1)))
      
        output$histogram_plot <- renderPlot(anova_ggObj)
        
        # ANOVA
        
        #bee_lm <- lm(length ~ species, data = prob_lengths)
        bee_aov <- aov(length ~ species, data = prob_lengths)
        aov_summary <- summary(bee_aov)
        bee_summary <- do.call(rbind.data.frame, summary(bee_aov))
        rownames(bee_summary) <- c("Species", "Residuals")
        
        
        # Tables
        colnames(summary_stats) <- c("Species", "Mean", "Std. Deviation", "Std. Error", "N")
#        output$mean_table <- renderTable(summary_stats)
        
#        output$anova_table <- renderTable(
#          bee_summary, rownames = TRUE
#        )
        
        output$anova_tbl <- renderDT(datatable(bee_summary,
                                               class = "compact",
                                               rownames = TRUE,
                                               options = list(dom = "t")) %>% 
                                     formatRound(columns = 2:6, digits = 2))
        output$summary_tbl = renderDT(datatable(summary_stats, 
                                        class = "compact",
                                        rownames = FALSE,
                                        options = list(dom = 't')) %>% 
                                formatStyle("Species", fontStyle = "italic"))
        updateActionButton(session, inputId = "anova_update", label = "Update")

      } else {
        upload_warning()
      }
    })

# Linear Regression -------------------------------------------------------

    regression_data <- reactive({
      upload_file(input$datafile)
    })

    ##  Code to build menus from column headers 
    ## https://blog.efpsa.org/2019/04/24/7-easy-steps-to-building-your-own-shiny-app-from-scratch/
    
    observe({
      updateSelectInput(session, 
                        "var1", 
                        choices = names(regression_data()),
                        selected = names(regression_data()[2]))
      updateSelectInput(session, 
                        "var2", 
                        choices = names(regression_data()),
                        selected = names(regression_data()[1]))
    })
    
    observeEvent(input$datafile, {
      updateActionButton(session, inputId = "update", label = "Analyze")
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
        output$regression_table <- renderDT(
          datatable(summary_df, 
                    rownames = TRUE,
                    class = "compact",
                    options = list(dom = "t")
                    ) %>% 
            formatRound(columns = 1:4, digits = 2)
          )
#        output$table <- renderTable(summary_df, rownames = TRUE)
        
        # Plot
        x_label <- sub("\\.", " ", input$var1)
        y_label <- sub("\\.", " ", input$var2)
        
        fit <- lm(var2 ~ var1, data = dat)
        dat$predicted <- predict(fit)
        ggObj <- ggplot(data = dat, aes(x = var1, y = var2)) +
          geom_point(color='gray60', size = 3) +
          labs(x = paste(x_label, "(mm)"),
               y = paste(y_label, "(mm)"),
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
                        color = "#9D2235", # Cardiac Red 
                        size = 1.5)
          
        
        }
        updateActionButton(session, inputId = "update", label = "Update")
        output$plot <- renderPlot(ggObj)
        
      } else {
        #showNotification("Upload a file first!", type = "error")
        upload_warning()
      }
    })
  }
