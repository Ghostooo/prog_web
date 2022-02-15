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
                                                               "Fill with mea n" = 2,
                                                               "Use KNN to fill" = 3,
                                                               "Keep them" = 4),
                                                  selected = 1),
                                      numericInput("prop_nas", label=h3("Minimal proportion per row to remove the NAs"),
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
                               tags$head(tags$style("#cat_table table {background-color: #8bb8bf; }", media="screen", type="text/css"))
                           )
                       )                   
                       ),
                   tabPanel(
                       "Data Visualization",
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
                   tabPanel(
                     "Train Models",
                       tabsetPanel(
                         tabPanel(
                           "Pre-training",
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
                                        actionButton(inputId="load_data", label="Train models"),
                                        offset = 3
                                 )
                               )
                             )
                         ),
                         tabPanel(
                           "Logistic Regression"
                         ),
                         tabPanel(
                           "Linear Regression"
                         ),
                         tabPanel(
                           "Decision Tree"
                         )
                       )
                     ),
                   theme=shinytheme("cosmo"),
                   tags$head(tags$style("body {font-family: Times New Roman; font-size: 300%; }", media="screen", type="text/css"))
        )
)
