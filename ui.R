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
                                                   value = 0.6),
                                      numericInput("prop_col_nas", label=h3("Minimal proportion per column to remove the NAs"),
                                                   value = 0.6)
                               ),
                               fluidRow(
                                      checkboxGroupInput(inputId = "pre_pros", label = h3("Pre-Processing"), 
                                                         choices = list("Scale Data" = 1,
                                                                        "Center Data" = 2,
                                                                        "Replace Outiliers with mean (more on Data Visualization)" = 4),
                                                         selected = 1)
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
                                   fluidRow(
                                       column(8,
                                              plotOutput(outputId = "boxplot"),
                                              offset = 1)
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
                       tabPanel("Data Visualization for two dimensions",
                                sidebarPanel(
                                  uiOutput("bi.dim.vari.choix.1.quant"),
                                  uiOutput("bi.dim.vari.choix.2.quant")
                                ),
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel(
                                      "Scatter Plot",
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
                       sidebarLayout(
                        
                           
                             sidebarPanel(
                                 fluidRow(
                                   uiOutput("target_choices")
                                 ),
                               fluidRow(
                                 selectInput(inputId = "balancing_choice", label=h3("Data Balancing"),
                                             choices=list("Under Sampling" = 1,
                                                          "Over Sampling" = 2,
                                                          "Both" = 3,
                                             selected = 1)
                                )
                               ),
                               fluidRow(
                                 numericInput("n_sample", label = h3("Sample size"), value = -1),
                               ),
                               fluidRow(
                                 column(3,
                                        actionButton(inputId="load_and_train_data", label="Train models"),
                                        offset = 3
                                 )
                               ),fluidRow(

                                 sliderInput("n_train", label = h3("choose the percentage of training set (0-1)"), min=0, max=1, value=1, step = 0.01)
                               ),fluidRow(
                                 textOutput(outputId="acc_pct")
                               ),fluidRow( 
                                 numericInput("pruning", label = h3("give the height of the tree :"), value = -1),
                                 column(3,actionButton(inputId="prune_tree", label="prune models"),offset=3)
                               )
                             )
                         ,mainPanel(tabsetPanel(
                         tabPanel("Logistic Regression",),
                         tabPanel("Linear Regression"),
                         tabPanel("Decision Tree",plotOutput(outputId = "treeplot"),plotOutput(outputId = "pruning_plot"))))
                         )
                       
                     ),
                   theme=shinytheme("cosmo"),
                   tags$head(tags$style("body {font-family: Times New Roman; font-size: 300%; }", media="screen", type="text/css"))
        )
)
