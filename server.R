library(shiny)
library(ggplot2)
library(tidyverse)
library(GGally)
library(reshape2)
library(ggthemes)
library(Factoshiny)
library(outliers)
library(rpart)
library(ISLR)
source("functions.R")
library(rattle)
library(misty)
attach(Carseats)

# in order to install the impute package which is not in the CRAN.
if (!require("BiocManager"))
  install.packages("BiocManager")
if(!require("impute", quietly = TRUE))
  BiocManager::install("impute")
library(impute)
# ----------------------------------------------------------------


shinyServer(function(input, output) {
  
  df = reactiveValues(data = NULL,col=c())
  
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
    
    # transform string data into factors
    data[, map_lgl(data, function(x) is.character(x))] <- data[, map_lgl(data, function(x) is.character(x))] %>%
      map(function(x) as.factor(x))
    
    
    ## NA's
    if(input$nas_choice == 1){ # remove
      
      # keep only the rows with less than prop_nas % of NA values
      data <- data[na.prop(data)<=input$prop_col_nas,]
      data <- data[,na.prop(t(data))<=input$prop_col_nas]
      
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
  
  
  observeEvent(dataset(),{
    df$data=dataset()
  })
  
  output$quant_table <- renderTable({
    if(!is.null(df$data)){
      df$data %>%
        select_if(is.numeric)
    }
  }, colnames = TRUE)
  
  output$cat_table <- renderTable({
    if(!is.null(df$data)){
      df$data  %>%
        select_if(is.factor)
    }
  })
  
  output$boxplot <- renderPlot({
    if(!is.null(df$data)){
      df$data %>%
        select(input$uni_dim_choice_vizu_quant) %>%
        ggplot() +
        geom_boxplot(outlier.colour = "red", aes(x = unlist(df$data[, input$uni_dim_choice_vizu_quant]), fill = "orange")) +
        labs(x = NULL, y = NULL, title = paste("Boxplot of the ", input$uni_dim_choice_vizu_quant, "variable")) +
        guides(fill=FALSE) +
        theme_solarized()
    }
  })
  
  
  output$barplot <- renderPlot({
    if(!is.null(df$data)){
      categ_data <- df$data %>%
        select(input$uni_dim_choice_vizu_qual) %>%
        ggplot() +
        geom_bar(aes(x = unlist(df$data[, input$uni_dim_choice_vizu_qual]), fill = unlist(df$data[, input$uni_dim_choice_vizu_qual]))) +
        labs(x = NULL, y = NULL, title=paste("Barplot of the ", input$uni_dim_choice_vizu_qual, "variable")) +
        guides(fill = FALSE) +
        theme_solarized()
      categ_data
    }
  })
  
  output$pca_var <- renderPlot({
    if(!is.null(df$data)){
      res.PCA<-PCA(df$data %>%
                     select_if(is.numeric),graph=FALSE)
      plot.PCA(res.PCA,choix='var',title="Graphe des variables de l'ACP")
    }
  })
  
  output$pca_individ <- renderPlot({
    if(!is.null(df$data)){
      res.PCA<-PCA(df$data %>%
                     select_if(is.numeric),graph=FALSE)
      plot.PCA(res.PCA,title="Graphe des individus de l'ACP")
    }
  })
  
  output$nuagePoints <- renderPlot({
    # Simple nuage de point 
    
    if(!is.null(df$data)) {
      x.var = input$bi.dim.choice.1.vizu.quant
      y.var = input$bi.dim.choice.2.vizu.quant
      
      df$data %>%
        ggplot() +
        geom_point(aes(x=unlist(df$data[, x.var]),
                      y=unlist(df$data[, y.var])),
                  size = 4.5,
                  col = "blue") +
        labs(x = "",
             y = "",
             title=paste("Scatter plot of the", x.var, "and", y.var, "variables.")) +
        theme_solarized()
    }
    
  })
   
  output$hist <- renderPlot({
    options(digits=1)
    
    if(!is.null(df$data)) {
      x.var = input$bi.dim.choice.1.vizu.quant
      y.var = input$bi.dim.choice.2.vizu.quant
    
      
      out <- Hmisc:::histbackback(x=unlist(df$data[, x.var]), y = unlist(df$data[, y.var]),
                           xlab = c(x.var, y.var), main = paste(x.var, "and", y.var), 
                           las = 2,
                           )
      
      barplot(-out$left, col="red", horiz=TRUE, space=0, add=TRUE, axes=FALSE)
      barplot(out$right, col="blue", horiz=TRUE, space=0, add=TRUE, axes=FALSE)
    }
  })
  
  # In tabSet Train Models
  output$target_choices <- renderUI({
    selectInput(inputId = "target_selected", choices = names(df$data),
                label = "Target Variable")
  })
  
  
  # the select bar for the quantitative var to plot (as a boxplot)
  output$uni_dim_vari_choix_quant <- renderUI({
    if(!is.null(df$data)) {
      numericals <- df$data %>%
        select_if(is.numeric) %>%
        names()
      selectInput(inputId = "uni_dim_choice_vizu_quant", choices = numericals,
                  label = "Please choose the variable to plot", selected = numericals[1])
    }
    
  })
  
  
  # the two select bars for the quantitative variables to plot (Ex : Scatter Plot)
  output$bi.dim.vari.choix.1.quant <- renderUI({
    if(!is.null(df$data)) {
      numericals <- df$data %>%
        select_if(is.numeric) %>%
        names()
      selectInput(inputId = "bi.dim.choice.1.vizu.quant", choices = numericals,
                  label = "Please choose the variables to plot", selected = numericals[1])
    }
  })
  
  
  output$bi.dim.vari.choix.2.quant <- renderUI({
    if(!is.null(df$data)) {
      numericals <- df$data %>%
        select_if(is.numeric) %>%
        select(-toString(input$bi.dim.choice.1.vizu.quant)) %>%
        names()
      
      selectInput(inputId = "bi.dim.choice.2.vizu.quant",
                  choices = numericals,
                  label = "",
                  selected = numericals[1])
    }
  })
  
  output$uni_dim_vari_choix_qual <- renderUI({
    if(!is.null(df$data)){
      factors <- df$data %>%
        select_if(is.factor) %>%
        names()
      selectInput(inputId = "uni_dim_choice_vizu_qual", choices = factors,
                  label = "Please choose the variable to plot", selected = factors[1])
    }
  })
  
  observeEvent(df$data,{
    choices <- c(names(df$data))
    updateSelectInput(inputId = "col_name", choices = choices)
  })
  
  
  output$one_table=renderTable({
    if(!is.null(df$data)){
      if(!is.null(input$col_name)){
        df$data %>% select(input$col_name)
      }
     
    }
  })
  
  output$na_pct=renderText({
    if(!is.null(input$col_name)){
      if(input$col_name!="nothing"){
        vec = is.na(df$data[,input$col_name])
        pct=as.character((sum(vec)/length(vec))*100)
        print("ok")
        print(pct)
        paste("the rate of Na value in this column is : ",as.character(pct),"%")
      }
    }
  })
  
  
  observeEvent(input$delet_column,{
    if(length(input$col_name) > 0){
      df$data[,input$col_name]=NULL
    }
  })
  
  models = reactiveValues(tree = NULL)
  
  observeEvent(input$load_and_train_data,{
    
    print(dim(df$data))
    print("here 1")
    if(input$n_train!=0){
      print("here 1")
      set.seed(2)
      train=sample(1:nrow(df$data),nrow(df$data)*input$n_train)
      print("here 2")
      test=-train
      print("here 3")
      train_data=df$data[train,]
      test_data_input=df$data[test,!(names(df$data) %in% c(input$target_selected))]
      test_data_output=df$data[test, c(input$target_selected)]
      models$tree=rpart(unlist(train_data[,input$target_selected])~.,
                        data=train_data[, !(names(df$data) %in% c(input$target_selected))])
    }
  })
  
  output$treeplot=renderPlot({
    fancyRpartPlot(models$tree)
  })
  
  
  
})