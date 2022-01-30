library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(
        navbarPage("Dashboard",
                   tabPanel(
                       "Data Cleaning",
                       sidebarPanel(
                               fluidRow(
                                      fileInput(inputId = "file", label = "Load a data file" , accept = c("text/plain", ".csv")),
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
                                                  selected = 1)
                               ),
                               fluidRow(
                                      checkboxGroupInput(inputId = "pre_pros", label = h3("Pre-Processing"), 
                                                         choices = list("Scale Data" = 1,
                                                                        "Center Data" = 2,
                                                                        "Balance Data" = 3,
                                                                        "Replace Outiliers with mean" = 4),
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
                                       column(8,
                                              tableOutput(outputId = "quant_table"),
                                              offset = 1)
                                   )
                               ),
                               # change style:    
                               tags$head(tags$style("#quant_table table {background-color: #8bb8bf; }", media="screen", type="text/css")),
                               tabPanel(
                                   "Categorical Variables",
                                   fluidRow(
                                       column(8,
                                              tableOutput(outputId = "cat_table"),
                                              offset = 1)
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
                                   fluidRow(
                                       column(8,
                                              plotOutput(outputId = "boxplots"),
                                              offset = 1)
                                   )
                               ),
                               tabPanel(
                                   "Categorical Variables",
                                   fluidRow(
                                        plotOutput(outputId = "barplots")
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
                   tabPanel("Train Models"),
                   theme=shinytheme("cosmo"),
                   tags$head(tags$style("body {font-family: Times New Roman; font-size: 300%; }", media="screen", type="text/css"))
        )
)
