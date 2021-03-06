list.of.packages <- c("shiny", "ggplot2", "tidyverse", "GGally", "reshape2",
                      "ggthemes", "outliers", "rattle", "misty", "tree", "FactoMineR", "ROSE",
                      "impute", "ggcorrplot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(shiny)
library(ggplot2)
library(tidyverse)
library(GGally)
library(reshape2)
library(ggthemes)
library(outliers)
library(rpart)
source("functions.R")
library(rattle)
library(misty)
library(tree)
library(MASS, include.only = 'stepAIC')
library(FactoMineR)
# for data balancing :
library(ROSE)
# in order to install the impute package which is not in the CRAN.
if (!require("BiocManager"))
  install.packages("BiocManager")
if(!require("impute", quietly = TRUE))
  BiocManager::install("impute")
library(impute)
library(ggcorrplot)
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

    # Pre-Processing :
    
    # transform string data into factors
    data[, map_lgl(data, function(x) is.character(x))] <- data[, map_lgl(data, function(x) is.character(x))] %>%
      map(function(x) as.factor(x))
    
    
    ## NA's
    if(input$nas_choice == 1){ # remove
      
      # keep only the rows with less than prop_nas % of NA values
      data <- data[na.prop(data)<input$prop_nas,]
      data <- data[,na.prop(t(data))<input$prop_col_nas]
      
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

    data[sapply(data, is.numeric)] <- scale(data[sapply(data, is.numeric)], center=cn, scale=sc)
    
    if("4" %in% input$pre_pros){
      data[sapply(data, is.numeric)] <- sapply(data[sapply(data, is.numeric)], remove_outliers)
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
  
  output$boxplot_without_outliers <- renderPlot({
    # The idea is to replace by mean such that,
    # an outlier is defined according to the equation : 
    # [Q1 - const_outliers*(Q3 - Q1), Q3 + const_outliers*(Q3 - Q1))]
    # This is a generic form of the most used equation to define outliers where we
    # fix const_outliers to 1.5 which gives us :
    # [Q1 - 1.5*(Q3 - Q1), Q3 + 1.5*(Q3 - Q1)]
    # Note: if const_outliers increase it will consider less outliers.
    # source : https://fr.wikipedia.org/wiki/Donn%C3%A9e_aberrante#Autres_appr%C3%A9ciations_de_la_variabilit%C3%A9
    if(!is.null(df$data)){
      df_outliers <- df$data[input$uni_dim_choice_vizu_quant]
      df_outliers <- apply(df_outliers, MARGIN=2, function(x) replace_outliers_k(x, k=input$const_outliers))
      # rendering the plot :
      data.frame(df_outliers) %>%
        ggplot() +
        geom_boxplot(outlier.colour = "red", aes(x = unlist(df_outliers), fill = "orange")) +
        labs(x = NULL, y = NULL, title = paste("Boxplot of the ", input$uni_dim_choice_vizu_quant, "variable")) +
        guides(fill=FALSE) +
        theme_solarized()
    }
  })
  
  
  observeEvent(input$apply_outliers, {
    if(!is.null(df$data)){
      df_outliers <- df$data[input$uni_dim_choice_vizu_quant]
      df_outliers <- apply(df_outliers, MARGIN=2, function(x) replace_outliers_k(x, k=input$const_outliers))
      df$data[input$uni_dim_choice_vizu_quant] <- df_outliers
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
      
      data.x = unlist(df$data[, x.var])
      data.y = unlist(df$data[, y.var])
      
      df$data %>%
        ggplot(aes(x=data.x, y=data.y)) +
        geom_point(size = 3.5, col = "blue") +
        # Droite de régression linéaire
        geom_smooth(method=input$in.bi.dim.choice.method, col="red") +
        labs(x = x.var,y = y.var,
             title=paste("Scatter plot of the", x.var, "and", y.var, "variables.")) +
        
        theme_solarized(base_size=15)
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
  
  output$box <- renderPlot({
    
    if(!is.null(df$data)) {
      x.var = input$bi.dim.choice.1.vizu.quant
      y.var = input$bi.dim.choice.2.vizu.quant
      
      data.x = unlist(df$data[, x.var])
      data.y = unlist(df$data[, y.var])
      
      
      qplot(x=data.x, y=data.y, xlab=x.var, ylab=y.var,
            geom=c("boxplot", "jitter"), fill=data.x) +
        theme(legend.title=element_blank())
    }
    
  })
  
  # Calcul et affichage du coefficient de corélation linéaire
  # ---
  output$correlation <- renderText({
    
    if(!is.null(df$data)) {
      x.var = input$bi.dim.choice.1.vizu.quant
      y.var = input$bi.dim.choice.2.vizu.quant
      
      data.x = unlist(df$data[, x.var])
      data.y = unlist(df$data[, y.var])
      
      coeff.tmp <- cov(data.x, data.y)/(sqrt(var(data.x)*var(data.y)))
      paste("Coefficient de corrélation linéaire =", round(coeff.tmp,digits = 2))
    }
    
  })
  
  # In tabSet Train Models
  output$target_choices <- renderUI({
    if(!is.null(df$data)){
    selectInput(inputId = "target_selected", choices = names(df$data  %>%
                                                               select_if(is.factor) ),
                label = "Target Variable")
  }})
  

  
  
  output$target_choices_balancing <- renderUI({
    if(!is.null(df$data)){
      selectInput(inputId = "target_selected_balancing", choices = names(df$data %>% mutate_if(is.character, factor) %>%
                                                                           select_if(~ nlevels(.) == 2)),
                  label = "Variable to balance",
                  width = "100%")      
    }
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
  
  output$out.bi.dim.choice.method <- renderUI({
    if(!is.null(df$data)) {
      methods <- c("lm", "glm", "loess")
      
      selectInput(inputId = "in.bi.dim.choice.method",
                  choices = methods,
                  label = "Choose method",
                  selected = methods[1])
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
  
  # training the tree 
  
  observeEvent(input$load_and_train_data,{
    
    if(input$n_train!=0 && !(is.null(df$data))){
      set.seed(2)
      train=sample(1:nrow(df$data),nrow(df$data)*input$n_train)
      test=-train
      train_data=df$data[train,]
      test_data_input=df$data[test,!(names(df$data) %in% c(input$target_selected))]
      test_data_output=df$data[test, c(input$target_selected)]
      
      models$tree=rpart(unlist(train_data[,input$target_selected])~.,
        maxdepth = input$max_d,
        minsplit=input$min_s,
        minbucket=7,data=train_data[, !(names(df$data) %in% c(input$target_selected))])
      
      
      predict.test=predict(models$tree,test_data_input,type="class")
      predict.test=as.character(predict.test)
      test_data_output=as.character(unlist(test_data_output))
      
      
      output$acc_pct=renderText({  paste("accuracy  : ",as.character(mean(predict.test!=test_data_output)*100),"%")})
      pr=models$tree$cptable

      output$pruning_plot=renderPlot({
        plot(pr[,"xerror"],type="b", ylab="taux d'erreur",xlab="nombre de noeud dans l'arbre")
        
        
      })
      
      

      updateSelectInput(inputId = "pruning", choices = as.numeric(pr[,"CP"]))
  
      
      output$cp_table=renderTable({pr})
      
      output$variable_importance <- renderTable({ data.frame(Variable=names(models$tree$variable.importance),
                                                             Importance=models$tree$variable.importance) })
      
      
      p.rpart <- printcp(models$tree, digits=3)
      rel.error <- p.rpart[, 3L]
      nsplit <- p.rpart[, 2L]
      method <- models$tree$method
      if (!method == "anova") 
        warning("may not be applicable for this method")
      output$rsq_plot <- renderPlot({ plot(nsplit, 1 - rel.error, xlab = "Number of Splits", 
           ylab = "R-square", ylim = c(0, 1), type = "o", main="R² Values") })
    }
  })
  
  
  observeEvent(input$prune_tree,{
    if(input$pruning!=1 & !is.null(models$tree)){
  
      models$tree=prune(models$tree, input$pruning)
      
      set.seed(2)
      train=sample(1:nrow(df$data),nrow(df$data)*input$n_train)
      test=-train
      
     
      test_data_input=df$data[test,!(names(df$data) %in% c(input$target_selected))]
      test_data_output=df$data[test, c(input$target_selected)]
      
      
      predict.test=predict(models$tree,test_data_input,type="class")
      predict.test=as.character(predict.test)
      
      test_data_output=as.character(unlist(test_data_output))
      
      
      output$acc_pct=renderText({ paste("accuracy  : ",as.character(mean(predict.test!=test_data_output)*100),"%")})
      
      
      
    }
  })
  
  
  # plot the tree 
  
  output$treeplot=renderPlot({
    
      
    if(!is.null(models$tree) && length(models$tree$cptable[,"nsplit"])> 1){
      
      fancyRpartPlot(models$tree)
    }else{
      
      ggplot() +
        annotate("text", x = 10,  y = 10,
                 size = 6,
                 label = "tree plot not available, possibly because the tree don't have splinted nodes or the model is not trained") + theme_void()
    }
      
  })
  
  
  output$data_balancing_before <- renderPlot({
    if(!is.null(df$data)){
      categ_data <- df$data %>%
        select(input$target_selected_balancing) %>%
        ggplot() +
        geom_bar(aes(x = unlist(df$data[, input$target_selected_balancing]), fill = unlist(df$data[, input$target_selected_balancing]))) +
        labs(x = NULL, y = NULL, title=paste("Barplot of the ", input$target_selected_balancing, "variable")) +
        guides(fill = FALSE) +
        theme_solarized()
      categ_data
    }
  })
  
  df_balancing_ <- eventReactive(input$balancing_data_size, {
    df_tmp <- df$data
    df_tmp[sapply(df_tmp, is.numeric)] <- impute.knn(as.matrix(df_tmp[sapply(df_tmp, is.numeric)]))$data
    df_tmp[sapply(df_tmp, is.factor)] <- lapply(df_tmp[sapply(df_tmp, is.factor)], addNA)
    data_to_use <- df_tmp
    data_balancing_formula <- as.formula(paste(input$target_selected_balancing,
                                               paste(names(data_to_use)[!names(data_to_use) %in% c(input$target_selected_balancing)], collapse = " + "),
                                               sep=" ~ "))
    df_balancing <-  ROSE(data_balancing_formula,
                          data=data_to_use,
                          N=input$balancing_data_size,
    )$data
    df_balancing
  })
  
  output$data_balancing_barplot <- renderPlot({
    if(!is.null(df$data)){
      
      categ_data <- df_balancing_() %>%
        select(input$target_selected_balancing) %>%
        ggplot() +
        geom_bar(aes(x = unlist(df_balancing_()[, input$target_selected_balancing]), fill = unlist(df_balancing_()[, input$target_selected_balancing]))) +
        labs(x = NULL, y = NULL, title=paste("Barplot of the ", input$target_selected_balancing, "variable")) +
        guides(fill = FALSE) +
        theme_solarized()
      categ_data
    }
  })
  
  output$balancing_size <- renderUI({
    
    if(!is.null(df$data)){
      n_lvl1 <- (table(df$data[, input$target_selected_balancing])[1]) %>% as.numeric()
      n_lvl2 <- (table(df$data[, input$target_selected_balancing])[2]) %>% as.numeric()
      if(input$balancing_choice == '1'){
        val <- abs(n_lvl1-n_lvl2) %>% as.numeric()
        val <- nrow(df$data)%/%2 + val
        m <- nrow(df$data)
        mi <- 2
      }else{
        val <- abs(n_lvl1-n_lvl2) %>% as.numeric()
        val <- nrow(df$data) + val
        m <- nrow(df$data)*2
        mi <- nrow(df$data)
      }
      sliderInput("balancing_data_size",
                  max = m,
                  min=mi,
                  value=val,
                  step = 1,
                  animate = TRUE,
                  label="",
                  width = "100%"
      )
    }
  })
  
  
  output$explain_balancing_method <- renderText({
    
    if(input$balancing_choice == "1"){
      # Under Sampling
      text <- paste("<u><h2>Choosed: <b>Under-Sampling</b></h2></u>",
                    "<i>This method works with majority class.",
                    "It reduces the number of observations from majority class to make the data set balanced.",
                    "The constant <b>N</b> represents the size of the output dataset",
                    "so basically if you want a proportional dataset juste take (default value):",
                    "<br><br><b>N = size_of_dataset - (size_majority_class - size_minority_class)</b><br><br>",
                    "which means that it will undersample the size_majority until N so it will",
                    "remove the difference between the majority and minority classes.</i>")
    }

    if(input$balancing_choice == "2"){
      # Over Sampling
      text <- paste("<u><h2>Choosed: <b>Over-Sampling</b></h2></u>",
                    "<i>This method works with minority class.",
                    "It increases the number of observations of the minority class to make the data set balanced.",
                    "The constant <b>N</b> represents the size of the output dataset",
                    "so basically if you want a proportional dataset juste take (default value):",
                    "<br><br><b>N = size_of_dataset + (size_majority_class - size_minority_class)</b><br><br>",
                    "which means that it will oversample the size_minority until N so it will",
                    "add the difference between the majority and minority classes.</i>")
    }
    
    if(input$balancing_choice == "3"){
      # Both oversampling and undersampling
      text <- paste("to be completed.")
    }

    return(text)
  })
  
  
  output$note_balancing_data <- renderText({
    text <- paste('<br><br><b><u>Note:</u> the results are not fixed because of the probability of resampling from the rare class. If missing and method is either "over" or "under" this proportion is determined by oversampling or, respectively, undersampling examples so that the sample size is equal to N.</b>')
  })
  

  
  observeEvent(input$apply_balancing, {
    if(!is.null(df$data) && !is.null(df_balancing_())){
      df$data <- df_balancing_()
    } 
  })
  
  
  output$corr_matrix <- renderPlot({
    if(!is.null(df$data)){
      Correlation <- cor(df$data %>% select_if(is.numeric))
      ggcorrplot(Correlation, lab=TRUE, title="",
                 legend.title = "Correlation")      
    }
  })
  
  
  
  
  output$target_choices_LR <- renderUI({
    selectInput(inputId = "target_selected_LR", choices = names(df$data),
                label = "Target Variable")
  })
  
  
  
  
  
  output$target_selected_lr_ui <- renderUI({
    if(!is.null(df$data)){
      selectInput("target_selected_lr",
                  label = "Target Variable",
                  choices = names(df$data %>% select_if(is.numeric) %>% select_if(~ sum(is.na(.)) == 0)))      
    }
  })
  
  output$features_selected_lr_ui <- renderUI({
    if(!is.null(df$data)){
      selectInput("features_selected_lr",
                  label = h3("Choose features to train with"), 
                  multiple = TRUE,
                  choices = names(df$data))      
    }
  })
  
  output$target_choices_LOR <- renderUI({
    data.LOR_ <- df$data
    data.LOR_[sapply(data.LOR_, is.character)] <- lapply(data.LOR_[sapply(data.LOR_, is.character)], 
                                                         as.factor)
    selectInput(inputId = "target_selected_LOR", choices = names(data.LOR_ %>% select_if(is.factor)),
                label = "Target Variable")
  })
  
  output$features_selected_lor_ui <- renderUI({
    if(!is.null(df$data)){

      selectInput("features_selected_lor",
                  label = h3("Choose features to train with"), 
                  multiple = TRUE,
                  choices = names(df$data))
    }
  })
  
  observeEvent(input$train_lr, {
    if(!is.null(df$data)){
      if(is.null(input$features_selected_lr)) {
        data.LR <- df$data
      } else {
        print(input$features_selected_lr)
        data.LR <- df$data[, c(input$target_selected_lr, input$features_selected_lr)]
      }
      
      data.LR <- data.LR %>% select_if(~ sum(is.na(.)) == 0)
      set.seed(2)
      train=sample(1:nrow(data.LR), nrow(data.LR)*input$n_train_lr)
      test=-train
      train_data=data.LR[train, !(names(data.LR) %in% c(input$target_selected_lr))]
      # View(train_data)
      test_data_input=data.LR[test, !(names(data.LR) %in% c(input$target_selected_lr))]
      test_data_output=data.LR[test, c(input$target_selected_lr)]
      
      model.LR <- lm(unlist(data.LR[train, input$target_selected_lr]) ~ ., data=train_data)
      
      sum_model_LR <- summary(model.LR)
      
      output$LR_model <- renderTable({
        temp <- data.frame(Feature=rownames(sum_model_LR$coefficients), sum_model_LR$coefficients)
        names(temp) <- c("Feature", "Coefficient", "Standard Error",
                         "t-Value", "p-Value")
        temp
      })
      
      output$LR_metrics <- renderText({
        paste("<u>R² : </u> <b>", sum_model_LR$r.squared,"</b><br><br>",
              "<u>Adjusted R² : </u> <b>", sum_model_LR$adj.r.squared,"</b>\n",
              collapse = " ")
      })
      
      # Application of a step Regression
      
      step_lm <- stepAIC(model.LR, direction="both")

      train.var <- names(coefficients(step_lm))[!(names(coefficients(step_lm)) %in% "(Intercept)")]

      # model.adjusted.LR <- lm(unlist(data.LR[train, input$target_selected_lr]) ~ ., data=train_data[, train.var])
      # 
      sum_step_model_LR <- summary(step_lm)

      output$LR_step_model <- renderTable({
        temp <- data.frame(Feature=names(coefficients(step_lm)), sum_step_model_LR$coefficients)

        names(temp) <- c("Feature", "Coefficient", "Standard Error",
                         "t-Value", "p-Value")
        temp
      })

      output$LR_step_metrics <- renderText({
          paste("<u>R² : </u> <b>", sum_step_model_LR$r.squared,"</b><br><br>",
                "<u>Adjusted R² : </u> <b>", sum_step_model_LR$adj.r.squared,"</b>\n",
                collapse = " ")
      })
      
      
    }
  })
  
  observeEvent(input$load_and_train_data_LOR,{
    
    if(input$n_train_LOR!=0 && !(is.null(df$data))){
      
      if(is.null(input$features_selected_lor)) {
        data.LOR <- df$data
      } else {
        data.LOR <- df$data[, c(input$target_selected_LOR, input$features_selected_lor)]
      }
      
      data.LOR[sapply(data.LOR, is.character)] <- lapply(data.LOR[sapply(data.LOR, is.character)], 
                                             as.factor)
      set.seed(2)
      train=sample(1:nrow(data.LOR), nrow(data.LOR)*input$n_train_LOR)
      test=-train
      train_data=data.LOR[train, !(names(data.LOR) %in% c(input$target_selected_LOR))]
      # View(train_data)
      test_data_input=data.LOR[test, !(names(data.LOR) %in% c(input$target_selected_LOR))]
      test_data_output=data.LOR[test, c(input$target_selected_LOR)]
      model.LOR <- glm(unlist(data.LOR[train, input$target_selected_LOR]) ~ ., 
                       family="binomial", data=train_data)
      print(names(summary(model.LOR)$coefficients))
      print(summary(model.LOR)$coefficients[,1])
      output$LOR_model <- renderTable({
        data.frame("Variable"=data.frame(summary(model.LOR)$coefficients) %>% rownames(),
                   "Coefficient"=summary(model.LOR)$coefficients[,1],
                   "Standard Error"=summary(model.LOR)$coefficients[,2],
                   "z-Value"=summary(model.LOR)$coefficients[,3],
                   "Proba.inf.|z|"=summary(model.LOR)$coefficients[,4])
      })
      
      output$LOR_metrics <- renderText({
        c("<u>Dispersion :</u> ", summary(model.LOR)$dispersion, "<br><br>")
      })
      
      
      output$residuals_LOR <- renderText({
        paste(
          "<h2>Residuals distribution: </h2>",
          "<div style='border: 1px solid black;'>",
          "<u><b>Q1: </b></u>", quantile(model.LOR$residuals)[[1]],
          "<br>",
          "<u><b>Q2: </b></u>", quantile(model.LOR$residuals)[[2]],
          "<br>",
          "<u><b>Median (Q3): </b></u>", quantile(model.LOR$residuals)[[3]],
          "<br>",
          "<u><b>Q4: </b></u>", quantile(model.LOR$residuals)[[4]],
          "<br>",
          "<u><b>Mean: </b></u>", mean(model.LOR$residuals),
          "<br>",
          "<u><b>Standard deviation: </b></u>", sd(model.LOR$residuals),
          "</div>")
      })
      
    }
  })
  
  

  
})
