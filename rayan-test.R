library(shiny)
library(tidyverse)


ui=fluidPage(
  
  navbarPage("RAZ",tabPanel("Data prep",
                            
      sidebarLayout(
      
        sidebarPanel(
          fileInput(inputId = "file1", label="choose CSV file",accept = c("text/plain",".csv")),
          selectInput(inputId="col_name", label = "choose a column",choices = c("nothing")),
          checkboxInput("cb_1","remouve Na value for the selected column"),
          actionButton(inputId="apply_data", label="apply change")
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("data",tableOutput(outputId ="contents")),
            tabPanel("Categorical data",tableOutput(outputId ="c_data")),
            tabPanel("Numeric data",tableOutput(outputId ="n_data")),
            tabPanel("the chosen one",tableOutput(outputId ="ch_data"))
          )
        )
      
    )
  ),tabPanel("Data plot",sidebarLayout(
              
              sidebarPanel(
                selectInput(inputId="col_name_2", label = "choose a column",choices = c("nothing")),
                actionButton(inputId="apply_data_2", label="apply change")
              ),
              mainPanel(
                tabsetPanel(
                  tabPanel("visualisation",plotOutput(outputId = "barplots"))
                )
                )
              )
             ),tabPanel("Data ML"),tabPanel("About")
  )
  

)


variable_checked_with_no_NA_value=c()


server=function(input,output){
  
  
  data=reactive({
    file=input$file1
    if(is.null(file)) return(NULL)
    read.csv(file$datapath,header=TRUE, stringsAsFactors = TRUE)
  })
  
  output$contents=renderTable({
      data()
  })
  
  output$n_data=renderTable({
    data() %>%
      select_if(is.numeric)
  },colnames = TRUE)
  
  output$c_data=renderTable({
    data() %>%
      select_if(is.factor)
  })
  
  output$ch_data=renderTable({
      data() %>%
        select(input$col_name)
  })
  
  observeEvent(data(),{
    choices <- c("nothing",names(data()))
    updateSelectInput(inputId = "col_name", choices = choices)
    updateSelectInput(inputId = "col_name_2", choices = choices)
 })
  
  
  output$barplots=observeEvent(input$apply_data_2,{
    
    if(input$col_name_2!="nothing"){
      
      print(sapply(data(), mode))
      print(input$col_name_2)
      
      plotOutput(hist(data()[[input$col_name_2]]))
      
      #if(is.factor(data()[input$col_name_2])){
        
      #}else{
        #if(is.numeric(data()[input$col_name_2])){
         
        #}
      #}
    }
  })
  
  observeEvent(input$apply_data,{
    if(input$cb_1==TRUE){
      if(length(variable_checked_with_no_NA_value)==0){
        variable_checked_with_no_NA_value=c(input$col_name)
        output$contents=renderTable({
          data() %>%
            drop_na(variable_checked_with_no_NA_value)
        })
      }else{
          if(! input$col_name %in%  variable_checked_with_no_NA_value){
            variable_checked_with_no_NA_value=c(variable_checked_with_no_NA_value,input$col_name)
            
            output$contents=renderTable({
              data() %>%
              drop_na(variable_checked_with_no_NA_value)
            })
          }
      }
    }else{
      variable_checked_with_no_NA_value = variable_checked_with_no_NA_value[! variable_checked_with_no_NA_value %in% c(input$col_name)]
      output$contents=renderTable({
        data() %>%
          drop_na(variable_checked_with_no_NA_value)
      })
    }
  })

  }

shinyApp(ui=ui,server=server)