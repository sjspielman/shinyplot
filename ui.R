library(shiny)
library(tidyverse)
library(colourpicker)


# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Visualize simple datasets with ggplot2"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

    # Input dataset in CSV                           
    fileInput("datafile", "Choose CSV File for plotting",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv")
        ),


    # Horizontal line ----
    tags$hr(),

    # Input: Type of plot to create
 #  conditionalPanel(condition = "output.userdata_exists",
#     radioButtons("geom", "Type of Plot to Create:",
#                 choices=c("Histogram" = "histogram",
#                 "Single Boxplot" = "singleboxplot",
#                 "Multiple Boxplots" = "multipleboxplot",
#                 "Jitter plot" = "jitter",
#                 "Bar plot to summarize quantitative data (i.e., mean with standard deviation)" = "barquant",    
#                 "Bar plot to display count data of a categorical variable" = "barcount",
#                 "Scatterplot" = "scatter")),     

    radioButtons("dataviz", "Type of data to visualization to create:",
                choices=c("Distribution of a quantitative variable" = "quant",
                          "Distribution of a quantitative variable, across categories of a categorical variable" = "multquant",
                          "Barplot: Distribution of a categorical variable (i.e., counts)" = "counts",
                          "Scatterplot: Relationship between two quantitative variables" = "scatter")),  
                
    uiOutput("selectplot"),
    
    uiOutput("selectvars"),
    
#     conditionalPanel(condition = "input.geom == 'scatter'",
#         uiOutput("xvar"),
#         uiOutput("yvar")),
#         
#     conditionalPanel(condition = "input.geom == 'barcount'",
#         uiOutput("countvar")),
#             
#     conditionalPanel(condition = "input.geom != 'barcount' | input.geom != 'scatter'",
#          uiOutput("mainvar")),
#         
#     conditionalPanel(condition = "input.geom == 'multipleboxplot' | input.geom == 'jitter' | input.geom == 'barquant'",
#         uiOutput("catvar")),
        



#         checkboxInput(inputId = "bestfit",
#             label = "Show the line of best fit for a *scatterplot*?",
#             value = TRUE,
#             width = NULL)
    ## ee!!
    colourInput("yaycolor", "Select color", value = "seagreen"),
                   
  actionButton("go", "Go!")  
  
  
  ),



    # Main panel for displaying outputs ----
    mainPanel(
      verbatimTextOutput("summary"),
      plotOutput("mahplot")
    )
  )
)
