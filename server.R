library(shiny)
library(ggplot2)
library(tidyverse)
library(GGally)
library(reshape2)
library(ggthemes)
library(Factoshiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    dataset <- eventReactive(input$load_data, {
        inFile <- input$file
        if(is.null(inFile)) return(NULL)
        read.csv(inFile$datapath, header=input$file_has_header, stringsAsFactors = TRUE)
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
