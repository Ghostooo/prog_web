library(shiny)
library(shinythemes)

library(shinyjs)
library(shinyWidgets)

# Define UI for application that draws a histogram
shinyUI(
        navbarPage("Dashboard",
                   tabPanel(
                       "Data Cleaning",
                       sidebarPanel(
                         tags$h1(tags$b(tags$u("Customize your Pipeline", icon("fa-thin fa-filter")))),
                         useShinyjs(),
                         panel(style = "overflow-y:scroll; height: 500px; position:relative; align: centre; #load_data{ background-color:'red' }",
                               fluidRow(
                                    radioButtons(inputId="file_type", tags$i("Data file separator"), c(CSV=",", CSV2=";", Excel="\t")),
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
                                 selectInput(inputId="col_name", label = h3("choose a column to show or to delete"), choices = c(""), multiple = TRUE)
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
                       fluidRow(
                         column(3,
                                actionButton(inputId="load_data", label="Load Dataset",
                                             icon("fa-thin fa-database"), 
                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4; box-shadow: 2px 2px 2px 2px black;"),
                                offset = 3
                         )
                       )
                       ),
                       mainPanel(
                           tabsetPanel(
                               tabPanel(
                                   "Quantitative Variables",
                                   fluidRow(
                                     useShinyjs(),
                                     column(8,
                                            panel(style = "overflow-y:scroll; height: 700px; position:relative; align: centre",
                                                  tableOutput(outputId = "quant_table")),
                                            offset = 2)
                                   )
                               ),
                               # change style:    
                               tags$head(tags$style("#quant_table table {background-color: #8bb8bf; }", media="screen", type="text/css")),
                               tabPanel(
                                   "Categorical Variables",
                                   fluidRow(
                                     useShinyjs(),
                                     column(8,
                                            panel(style = "overflow-y:scroll; height: 700px; position:relative; align: centre",
                                                  tableOutput(outputId = "cat_table")),
                                            offset = 2)
                                   )
                               ),
                               # change style:    
                               tags$head(tags$style("#cat_table table {background-color: #8bb8bf; }", media="screen", type="text/css")),
                               tabPanel(
                                 "choosen variable",
                                 fluidRow(
                                   useShinyjs(),
                                   column(8,
                                          panel(style = "overflow-y:scroll; height: 700px; position:relative; align: centre",
                                            tableOutput(outputId = "one_table")),
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
                                    tabPanel("Boites Parallèles",
                                             fluidRow(
                                               column(6, offset=2,
                                                      plotOutput(outputId = "box"))
                                             ))
                                  )  
                              )
                      ),
                     )
                   ),
                   tabPanel("Correlation Matrix",
                            column(12,
                                   align = "center",
                                   tags$h2(tags$b("Correlation matrix of the numeric variables")),
                                   plotOutput("corr_matrix",
                                              width = "700px",
                                              height = "700px")
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
                                        column(2,
                                               uiOutput("target_choices_LOR"),
                                               sliderInput("n_train_LOR", label = h3("choose the percentage of training set (0-1)"), min=0, max=1, value=0.8, step = 0.01),
                                               uiOutput("features_selected_lor_ui"),
                                               actionButton(inputId="load_and_train_data_LOR", label="Train models")),
                                        column(8,
                                               align="center",
                                               tags$div(
                                                 tableOutput("LOR_model"),
                                                 style="border: 1px solid black; box-shadow: 10px 5px 2px black; background-color: #f7e9d4;"
                                               )
                                        ),
                                        column(2,
                                               htmlOutput("LOR_metrics"),
                                               textOutput(outputId="acc_pct_LOR"),
                                               htmlOutput("residuals_LOR"))
                                      )
                                    )
                              )
                         ),
                         tabPanel("Linear Regression",tabsetPanel(
                           tabPanel("train model",
                                  fluidRow(
                                     column(2,
                                            uiOutput("target_selected_lr_ui"),
                                            sliderInput("n_train_lr", label = h3("choose the percentage of training set (0-1)"), min=0, max=1, value=0.8, step = 0.01),
                                            uiOutput("features_selected_lr_ui"),
                                            actionButton("train_lr", label="Train Model")),
                                     column(7,
                                            align="center",
                                            tags$div(
                                              tableOutput("LR_model"),
                                              style="border: 1px solid black; box-shadow: 10px 5px 2px black; background-color: #f7e9d4;"
                                              )),
                                     column(3,
                                            tags$h3(tags$u("Some metrics :")),
                                            htmlOutput("LR_metrics"))
                                  ),
                                  fluidRow(
                                    column(5,
                                           tags$h3(tags$u(tags$b("After stepAIC:")))
                                           )
                                  ),
                                  fluidRow(
                                    
                                    column(7,
                                           align="center",
                                           offset=2,
                                           tags$div(
                                             tableOutput("LR_step_model"),
                                             style="border: 1px solid black; box-shadow: 10px 5px 2px black; background-color: #f7e9d4;"
                                           )),
                                    column(3,
                                           tags$h3(tags$u("Some metrics :")),
                                           htmlOutput("LR_step_metrics"))
                                  ),
                                  tags$hr()
                         ))),
                         tabPanel("Decision Tree",tabsetPanel(
                           tabPanel("train model",
                                    
                                    fluidRow(
                                      
                                      
                                      column(2, 
                                             uiOutput("target_choices"),
                                             
                                             numericInput("max_d", label=h3("Maxdepth"), value=30),
                                             numericInput("min_s", label=h3("Minsplit (minimal number of objects to split)"), value=1),
                                             numericInput("min_b", label=h3("minbucket (minimal number of observation in a leaf)"), value=1),
                                                          
                                                          
                                             actionButton(inputId="load_and_train_data", label="Train models"),
                                             
                                             sliderInput("n_train", label = h3("choose the percentage of training set (0-1)"), min=0, max=1, value=0.8, step = 0.01),
                                             
                                             
                                             
                                             
                                             
                                             
                                             ),
                                      column(7,
                                             plotOutput(outputId = "treeplot"),
                                             plotOutput(outputId = "pruning_plot"),
                                             ),
                                      column(3,
                                             selectInput("pruning", label = h3("Give the complexity of the model from the table (cp column) in order to prune the tree:"), choices = c()),
                                             actionButton(inputId="prune_tree", label="prune models"),
                                             tableOutput(outputId = "cp_table"),
                                             textOutput(outputId="acc_pct")
                                             )
                                    ),
                                    fluidRow(
                                      column(12,
                                             align="center",
                                               column(2,
                                                      tags$h3(tags$b(tags$u("Variable Importance:"))),
                                                      tableOutput("variable_importance")),
                                               column(5,
                                                      plotOutput("rsq_plot")
                                               ),
                                             offset=2)
                                    )
                           )
                          
                           
                           
                         )
                                  
                                  
                                  
                                  ))
                         
                         
                       
                     ),
                   theme=shinytheme("cosmo"),
                   tags$head(tags$style("body {font-family: Times New Roman; font-size: 300%; }", media="screen", type="text/css"))
        )
)
