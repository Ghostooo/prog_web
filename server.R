library(shiny)
library(ggplot2)
library(tidyverse)
library(GGally)
library(reshape2)
library(ggthemes)
library(Factoshiny)
library(outliers)
source("functions.R")

# in order to install the impute package which is not in the CRAN.
if (!require("BiocManager"))
  install.packages("BiocManager")
if(!require("impute", quietly = TRUE))
  BiocManager::install("impute")
library(impute)
# ----------------------------------------------------------------


shinyServer(function(input, output) {

    dataset <- eventReactive(input$load_data, {
        inFile <- input$file
        if(is.null(inFile)) return(NULL)
        
        if(input$file_type == "\t"){
          data <- readxl::read_excel(inFile$datapath)
        }else{
          data <- read_delim(file = inFile$datapath,
                             col_names=input$file_has_header,
                             delim = input$file_type)
        }
        print(input$file_type)
        
        # Pre-Processing :
        
        ## NA's
        if(input$nas_choice == 1){ # remove
          data$na_prop <- apply(data, MARGIN=1, function(x) (sum(is.na(x))) / length(names(x)))
          # keep only the rows with less than prop_nas % of NA values
          data <- data[data$na_prop <= input$prop_nas,]
          data <- data %>%
            select(-na_prop)
        }else if(input$nas_choice == 2){ # fill with mean
          data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
        }else if(input$nas_choice == 3){ # use KNN imputation algorithm to fill the NAs
          data[sapply(data, is.numeric)] <- impute.knn(as.matrix(data[sapply(data, is.numeric)]))$data
        }
        # if it's "Keep them" it will return the dataset as it is.
        ## Normalisation
        ### Scaling & Centering
        sc = "1" %in% input$pre_pros
        cn = "2" %in% input$pre_pros
        print(sc)
        print(cn)
        data[sapply(data, is.numeric)] <- scale(data[sapply(data, is.numeric)], center=cn, scale=sc)
        
        if("4" %in% input$pre_pros){
          data[sapply(data, is.numeric)] <- sapply(data[sapply(data, is.numeric)], remove_outliers)
          print("outliers replaced")
        }
        
        # transform string data into factors
        data[, map_lgl(data, function(x) is.character(x))] <- data[, map_lgl(data, function(x) is.character(x))] %>%
          map(function(x) as.factor(x))
        return(data)
    })
    
    output$quant_table <- renderTable({
        dataset() %>%
            select_if(is.numeric)
    }, colnames = TRUE)
    
    output$cat_table <- renderTable({
        dataset() %>%
            select_if(is.factor)
    })
    
    output$boxplot <- renderPlot({
        dataset() %>%
            select(input$uni_dim_choice_vizu_quant) %>%
            ggplot() +
            geom_boxplot(outlier.colour = "red", aes(x = unlist(dataset()[, input$uni_dim_choice_vizu_quant]), fill = "orange")) +
            labs(x = NULL, y = NULL, title = paste("Boxplot of the ", input$uni_dim_choice_vizu_quant, "variable")) +
            guides(fill=FALSE) +
            theme_solarized()
    })
    
    
    output$barplot <- renderPlot({
        categ_data <- dataset() %>%
            select(input$uni_dim_choice_vizu_qual) %>%
            ggplot() +
            geom_bar(aes(x = unlist(dataset()[, input$uni_dim_choice_vizu_qual]), fill = unlist(dataset()[, input$uni_dim_choice_vizu_qual]))) +
            labs(x = NULL, y = NULL, title=paste("Barplot of the ", input$uni_dim_choice_vizu_qual, "variable")) +
            guides(fill = FALSE) +
            theme_solarized()
        categ_data
    })
    
    output$pca_var <- renderPlot({
        res.PCA<-PCA(dataset() %>%
                         select_if(is.numeric),graph=FALSE)
        plot.PCA(res.PCA,choix='var',title="Graphe des variables de l'ACP")
    })
    
    output$pca_individ <- renderPlot({
        res.PCA<-PCA(dataset() %>%
                         select_if(is.numeric),graph=FALSE)
        plot.PCA(res.PCA,title="Graphe des individus de l'ACP")
    })
    
    output$target_choices <- renderUI({
      selectInput(inputId = "target_selected", choices = names(dataset()),
                  label = "Target Variable")
    })
    
    
    # the select bar for the quantitative var to plot (as a boxplot)
    output$uni_dim_vari_choix_quant <- renderUI({
      numericals <- dataset() %>%
        select_if(is.numeric) %>%
        names()
      selectInput(inputId = "uni_dim_choice_vizu_quant", choices = numericals,
                  label = "Please choose the variable to plot", selected = numericals[1])
      
    })
    
    output$uni_dim_vari_choix_qual <- renderUI({
      factors <- dataset() %>%
        select_if(is.factor) %>%
        names()
      selectInput(inputId = "uni_dim_choice_vizu_qual", choices = factors,
                  label = "Please choose the variable to plot", selected = factors[1])
    })
    
    

})
