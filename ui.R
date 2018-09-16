library(shiny)
library(tidyverse)
library(colourpicker)


# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title
  titlePanel("Visualize simple data distributions with ggplot2"),
    helpText("Written by Stephanie J. Spielman.", (a("Source code and instructions",
        href="https://github.com/sjspielman/shinyplot"))),
        
  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(
        
         
    # Input dataset in CSV                           
    fileInput("datafile", "Choose CSV File for plotting",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv")
        ),


    # Horizontal line
    conditionalPanel("output.userdata_exists",
        {tags$hr()}),
   
    # Type of visualization (not necessarily geom!!)
    conditionalPanel("output.userdata_exists",
    radioButtons("dataviz", "Type of data to visualization to create:",
                choices=c("Distribution of a quantitative variable" = "quant",
                          "Distribution of a quantitative variable, across ONE categorical variable" = "multquant",
                          "Distribution of a quantitative variable, across TWO categorical variables" = "multquant2",
                          "Distribution of a categorical variable (barplot)" = "counts",
                          "Distribution of a categorical variable, across another categorical variable (barplot)" = "counts2",
                          "Relationship between two quantitative variables (scatterplot)" = "scatter"))  
    ),
    # Horizontal line
    
    conditionalPanel("output.userdata_exists",
        {tags$hr()}),
               
    # Select variables for any plot
    uiOutput("selectvars"),
    
    # Select geom for a quantitative plot (quant or multquant)     
    uiOutput("selectplot"),
    
    ## hurray color!!
    conditionalPanel("output.userdata_exists",
        {colourInput("yaycolor", "Select color:", value = "seagreen")}
    ),

    # Horizontal line
    conditionalPanel("output.userdata_exists",
        {tags$hr()}),
                       
    ## This is a go button, and furthermore, a deeply useful comment.
    conditionalPanel("output.userdata_exists",
        {actionButton("go", "Go!")}
    )),



    # Main panel for displaying outputs
    mainPanel(
        verbatimTextOutput("summary"),
      
        div(style = "height: 500px;",
            plotOutput("mahplot", height = "100%")
        ),
    
        uiOutput("download")

    )
  )
)
