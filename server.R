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


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    dataset <- eventReactive(input$load_data, {
        inFile <- input$file
        if(is.null(inFile)) return(NULL)
        data <- read.csv(inFile$datapath, header=input$file_has_header, stringsAsFactors = TRUE)
        
        
        # Pre-Processing :
        
        ## NA's
        if(input$nas_choice == 1){ # remove
          data <- na.omit(data)
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
    
    output$boxplots <- renderPlot({
        dataset() %>%
            select_if(is.numeric) %>%
            melt() %>%
            ggplot() +
            geom_boxplot(outlier.colour = "red", aes(x = value, fill = variable)) +
            facet_wrap(~variable, scales = "free") +
            labs(x = NULL) +
            theme_solarized()
    })
    
    
    output$barplots <- renderPlot({
        categ_data <- dataset() %>%
            select_if(is.factor)
        categ_data %>%
        melt(id.vars=names(categ_data)[1]) %>%
        ggplot(aes(x = value, fill=variable)) +
        geom_bar() +
        facet_wrap(~variable, scales = "free") +
        labs(x = NULL) +
        theme_solarized()
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
    

})
