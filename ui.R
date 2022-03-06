library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(
        navbarPage("Dashboard",
                   tabPanel(
                       "Data Cleaning",
                       sidebarPanel(
                               fluidRow(
                                    radioButtons(inputId="file_type", "Select the separation type of the data you are uploading", c(CSV=",", CSV2=";", Excel="\t")),
                                    fileInput(inputId = "file", label = "Load a data file", accept = c(".xls", ".csv", ".xlsx"))
                               ),
                               fluidRow(
                                   checkboxInput(inputId = "file_has_header", label = "File has header", value = TRUE)
                               ),
                               fluidRow(
                                      selectInput(inputId = "nas_choice", label=h3("NAs values"),
                                                  choices=list("Remove" = 1,
                                                               "Fill with mean" = 2,
                                                               "Use KNN to fill" = 3,
                                                               "Keep them" = 4),
                                                  selected = 1),
                                      numericInput("prop_nas", label=h3("Minimal proportion per row to remove the NAs"),
                                                   min = 0.0, max = 1.0, step=0.1, value = 0.6),
                                      numericInput("prop_col_nas", label=h3("Minimal proportion per column to remove the NAs"),
                                                   min = 0.0, max = 1.0, step = 0.1, value = 0.6)
                               ),
                               fluidRow(
                                      checkboxGroupInput(inputId = "pre_pros", label = h3("Pre-Processing"), 
                                                         choices = list("Scale Data" = 1,
                                                                        "Center Data" = 2,
                                                                        "Replace Outiliers with mean (more on Data Visualization)" = 4))
                                ),
                               fluidRow(
                                    numericInput("n_sample", label = h3("Sample size"), value = -1),
                               ),
                               fluidRow(
                                   column(3,
                                          actionButton(inputId="load_data", label="Load Dataset"),
                                          offset = 3
                                          )
    
                               ),
                               fluidRow(
                                 selectInput(inputId="col_name", label = h3("choose a column to show or to delete"),choices = c(""),multiple = TRUE)
                               ),
                               fluidRow(
                                 column(3,
                                        actionButton(inputId="delet_column", label="Delete selected column"),
                                        offset = 2
                                 )
                               ),fluidRow(
                                 textOutput(outputId="na_pct")
                               )
                       ),
                       mainPanel(
                           tabsetPanel(
                               tabPanel(
                                   "Quantitative Variables",
                                   fluidRow(
                                     column(5,
                                            ,
                                            offset = 2)
                                   ),
                                   fluidRow(
                                       column(8,
                                              tableOutput(outputId = "quant_table"),
                                              offset = 2)
                                   )
                               ),
                               # change style:    
                               tags$head(tags$style("#quant_table table {background-color: #8bb8bf; }", media="screen", type="text/css")),
                               tabPanel(
                                   "Categorical Variables",
                                   fluidRow(
                                       column(8,
                                              tableOutput(outputId = "cat_table"),
                                              offset = 2)
                                   )
                               ),
                               # change style:    
                               tags$head(tags$style("#cat_table table {background-color: #8bb8bf; }", media="screen", type="text/css")),
                               tabPanel(
                                 "choosen variable",
                                 fluidRow(
                                   column(8,
                                          tableOutput(outputId = "one_table"),
                                          offset = 1)
                                 )
                               ),
                               tags$head(tags$style("#one_table table {background-color: #8bb8bf; }", media="screen", type="text/css"))
                           )
                        )                   
                       ),
                   tabPanel(
                       "Data Visualization",
                       navbarPage("",
                         tabPanel("Visualization for one dimension",
                            tabsetPanel(
                               tabPanel(
                                   "Quantitative Variables",
                                   uiOutput("uni_dim_vari_choix_quant"),
                                   fluidRow(tags$u("Before replacing outliers with mean according to k constant :")),
                                   fluidRow(
                                       column(8,
                                              plotOutput(outputId = "boxplot"),
                                              offset = 1)
                                   ),
                                  fluidRow(
                                    tagList(tags$i("Fix the positive constant k to define outliers according to"),
                                            tags$a('this method',
                                              href = "https://fr.wikipedia.org/wiki/Donn%C3%A9e_aberrante#Autres_appr%C3%A9ciations_de_la_variabilit%C3%A9"),
                                            tags$i('(replace with mean).'))
                                  ),
                                  fluidRow(
                                    sliderInput("const_outliers",
                                                min=0,
                                                max = 5,
                                                value=1.5,
                                                step = 0.2,
                                                animate = TRUE,
                                                label="",
                                                width = "100%"
                                    )
                                  ),
                                  fluidRow(tags$u("After replacing outliers with mean according to k constant :")),
                                  fluidRow(
                                    column(8,
                                           plotOutput(outputId = "boxplot_without_outliers"),
                                           offset = 1
                                    )
                                  ),
                                  fluidRow(
                                    column(4,
                                           actionButton(inputId="apply_outliers", label="Apply on real data", width = "70%"),
                                           offset=4)
                                  ),
                                  fluidRow(
                                    tags$i('Note: this operation is irreversible.')
                                  )
                               ),
                               tabPanel(
                                   "Categorical Variables",
                                   uiOutput("uni_dim_vari_choix_qual"),
                                   fluidRow(
                                        plotOutput(outputId = "barplot")
                                   )
                               ),
                               tabPanel(
                                 "PCA",
                                 fluidRow(
                                   column(6,
                                          plotOutput(outputId = "pca_var")
                                          ),
                                   column(6,
                                          plotOutput(outputId = "pca_individ")
                                          )
                                   
                                 )
                               )
                          )
                       ),
                       tabPanel("Visualization for two dimensions",
                                sidebarPanel(
                                  uiOutput("bi.dim.vari.choix.1.quant"),
                                  uiOutput("bi.dim.vari.choix.2.quant"),
                                  fluidRow(
                                    column(8, textOutput("correlation"))
                                  )
                                ),
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel(
                                      "Scatter Plot",
                                      uiOutput("out.bi.dim.choice.method"),
                                      fluidRow(
                                        column(6, offset=2, 
                                          plotOutput(outputId = "nuagePoints")
                                        )
                                      )
                                    ),
                                    tabPanel(
                                      "Histogramme",
                                      fluidRow(
                                        column(6, offset=2, 
                                               plotOutput(outputId = "hist")
                                        )
                                      )
                                    ),
                                    tabPanel("Boites Parallèles")
                                  )  
                              )
                      )
                     )
                   ),
                   tabPanel(
                     "Train Models",
                       
                        
                           
                        tabsetPanel(
                         tabPanel("Balancing Method",
                                  fluidRow(
                                    column(6,
                                           uiOutput("target_choices_balancing"))
                                  ),
                                  fluidRow(
                                    tags$i(tags$b("Only for two levels targets."))
                                  ),
                                  fluidRow(tags$h3(tags$u("Before balancing:"))),
                                  fluidRow(
                                    column(6,
                                           plotOutput(outputId = "data_balancing_before")
                                    ),
                                    column(4,
                                           fluidRow(
                                             selectInput(inputId = "balancing_choice", label=h3("Data Balancing"),
                                                         choices=list("Under Sampling" = 1,
                                                                      "Over Sampling" = 2,
                                                                      selected = 2
                                                         )
                                             )
                                           ),
                                           fluidRow(
                                             uiOutput("balancing_size"),
                                             tags$i("please read the description below.")
                                           ),
                                           fluidRow(
                                             actionButton(inputId="apply_balancing", label="Apply On Real Data")
                                           ),
                                           offset = 1)
                                  ),
                                  fluidRow(tags$h3(tags$u("After balancing:"))),
                                  fluidRow(
                                         column(6,
                                                plotOutput(outputId = "data_balancing_barplot")
                                         )
                                  ),
                                  fluidRow(
                                    column(12,
                                           htmlOutput(outputId = "explain_balancing_method")
                                    ),
                                    column(12,
                                           htmlOutput(outputId = "note_balancing_data"))
                                  )
                         ),
                         tabPanel("Logistic Regression",tabsetPanel(
                           tabPanel("train model", 
                                    
                                    fluidRow(
                                      uiOutput("target_choices_LOR")
                                    )
                                    
                                    
                                    ,fluidRow(
                                      column(3,
                                             actionButton(inputId="load_and_train_data_LOR", label="Train models"),
                                             offset = 0
                                      )
                                      
                                    ),fluidRow(
                                      
                                      sliderInput("n_train_LOR", label = h3("choose the percentage of training set (0-1)"), min=0, max=1, value=1, step = 0.01)
                                      
                                    ),
                                    
                                    fluidRow(
                                      textOutput(outputId="acc_pct_LOR")
                                    )
                                    
                              ),
                           tabPanel("model params"),
                           tabPanel("model")
                           
                           
                         )),
                         tabPanel("Linear Regression",tabsetPanel(
                           tabPanel("train model", 
                                    
                                    fluidRow(
                                      uiOutput("target_choices_LR")
                                    )
                                     
                                    
                                    ,fluidRow(
                                       column(3,
                                              actionButton(inputId="load_and_train_data_LR", label="Train models"),
                                              offset = 0
                                       )
                                       
                                     ),fluidRow(
                                       
                                       sliderInput("n_train_LR", label = h3("choose the percentage of training set (0-1)"), min=0, max=1, value=1, step = 0.01)
                                       
                                     ),
                           
                                     fluidRow(
                                       textOutput(outputId="acc_pct_LR ")
                                     )
                                     
                           ),
                           
                           tabPanel("model params"),
                           tabPanel("model")
                           
                           
                         )),
                         tabPanel("Decision Tree",tabsetPanel(
                           tabPanel("train model",
                                    
                                    fluidRow(
                                      uiOutput("target_choices")
                                    )
                                    ,
                                    fluidRow(
                                      column(3,
                                             actionButton(inputId="load_and_train_data", label="Train models"),
                                             offset = 0
                                      )
                                    ),fluidRow(
                                      
                                      sliderInput("n_train", label = h3("choose the percentage of training set (0-1)"), min=0, max=1, value=1, step = 0.01)
                                      
                                    ),fluidRow(
                                      textOutput(outputId="acc_pct")
                                    )
                           ),
                           tabPanel("tree pruning"
                                    ,
                                    fluidRow( 
                                      numericInput("pruning", label = h3("Give the complexity of the model from the table (cp column) so that we could prune the tree :"), value = -1)
                                      
                                    ),fluidRow(
                                      column(3,actionButton(inputId="prune_tree", label="prune models"))
                                    )
                           ),tabPanel("model params"
                             
                             
                           ),
                           tabPanel("model",
                             plotOutput(outputId = "treeplot"),
                             plotOutput(outputId = "pruning_plot"),
                             column(8,tableOutput(outputId = "cp_table"),offset=3)
                           )
                          
                           
                           
                         )
                                  
                                  
                                  
                                  ))
                         
                         
                       
                     ),
                   theme=shinytheme("cosmo"),
                   tags$head(tags$style("body {font-family: Times New Roman; font-size: 300%; }", media="screen", type="text/css"))
        )
)
