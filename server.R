library(shiny)
library(colourpicker)
library(tidyverse)

server <- function(input, output) {

    
    #This function is repsonsible for loading in the selected file
    userdata <- reactive({
      infile <- input$datafile
      if (is.null(infile)) {
        # User has not uploaded a file yet
        return(NULL)
      }
      read_csv(infile$datapath)
    })
    
   output$userdata_exists <- reactive(!is.null(userdata()))
   outputOptions(output, "userdata_exists", suspendWhenHidden = FALSE)


    output$selectplot <- renderUI({
        
        if (is.null(userdata())) return(NULL)

        switch(input$dataviz, 
                "quant" = selectInput("whichquant", 
                        "Which type of plot would you like to make?", 
                        choices = c("Histogram", "Boxplot", "Jitter plot")),
                "multquant" = selectInput("whichquant", 
                        "Which type of plot would you like to make?", 
                        choices = c("Boxplot", "Jitter plot"))
                )
        ####### to do: we should be able to do this with barplots and error bars ########
        
#         if (input$dataviz == "quant")
#         {
#             selectInput("whichquant", 
#                         "Which type of plot would you like to make?", 
#                         choices = c("Histogram", "Boxplot", "Jitter plot"))
#         }      
#         
#         if (input$dataviz == "multquant")
#         {  
#              selectInput("whichquant", 
#                         "Which type of plot would you like to make?", 
#                         choices = c("Boxplot", "Jitter plot"))
#         }      
    })


    output$selectvars <- renderUI({
    
        
        input$dataviz
        if (is.null(userdata())) return(NULL)
        dfnames <- names(userdata())
        
        switch(input$dataviz, 
                "scatter" = list(
                                selectInput("xvar", "Select the X-axis (aka independent/predictor) variable:",dfnames),
                                selectInput("yvar", "Select the Y-axis (aka dependent/response) variable:",dfnames),
                                checkboxInput(inputId = "bestfit",
                                    label = "Show the line of best fit (i.e. regression line)?",
                                    value = TRUE,
                                    width = NULL
                                )),         
                "quant"   = selectInput("quantvar", "Select the *quantitative* variable whose distribution to visualize:",dfnames),
                "multquant" = list(
                                    selectInput("quantvar", "Select the *quantitative* variable whose distribution to visualize:",dfnames),
                                    selectInput("quantvar_cat", "Select the *categorical* variable to visualize across:",dfnames)
                                ),
                "counts"   = selectInput("countvar", "Select the *categorical* variable to visualize its count:",dfnames)                                        
            )
    })
 
 
 
# Generate a summary of the data
output$summary <- renderPrint({
    req(userdata())
    if (is.null(userdata())) return(NULL)
     summary(userdata())
})


observeEvent(input$go,  {

    finaldata <- isolate(userdata())
    output$mahplot <- renderPlot( {


        theme_set(theme_classic() + theme(axis.text = element_text(size=15), axis.title = element_text(size=18)))
 
 
        viz <- isolate(input$dataviz)
        geom_quant <- isolate(input$whichquant)
        thecolor <- isolate(input$yaycolor)
    

        ################ Single distribution ##################
        if (viz == "quant" & geom_quant == "Histogram")
        {
            quantvar2 <- isolate(input$quantvar)
            p <- ggplot(finaldata, aes_string(x = quantvar2)) + geom_histogram(color = "black", fill = thecolor)
        }
        if (viz == "quant" & geom_quant == "Boxplot")
        {
            quantvar2 <- isolate(input$quantvar)
            p <- ggplot(finaldata, aes_string(y = quantvar2)) + geom_boxplot(fill = thecolor)
        }
        if (viz == "quant" & geom_quant == "Jitter plot")
        {
            quantvar2 <- isolate(input$quantvar)
            p <- ggplot(finaldata, aes_string(y = quantvar2)) + geom_jitter(aes(x=""), color = thecolor, width=0.05)
        }
        ########################################################

        ################ Multiple distributions ##################
        if (viz == "multquant" & geom_quant == "Boxplot")
        {
            quantvar2 <- isolate(input$quantvar)
            quantvar_cat2 <- isolate(input$quantvar_cat)
            
            p <- ggplot(finaldata, aes_string(x = quantvar_cat2, y = quantvar2, fill = quantvar_cat2, group = quantvar_cat2)) + geom_boxplot()
        }
        if (viz == "multquant" & geom_quant == "Jitter plot") ## todo: color by group such that it forces factor in aes_string?
        {
            quantvar2 <- isolate(input$quantvar)
            quantvar_cat2 <- isolate(input$quantvar_cat)
            p <- ggplot(finaldata, aes_string(x = quantvar_cat2, y = quantvar2, group = quantvar_cat2)) + geom_jitter(color = thecolor)
        }
        ########################################################


        ################ Barplot for counts ##################
        if (viz == "counts")
        {
            countvar <- isolate(input$countvar)
            
            p <- ggplot(finaldata, aes_string(x = countvar)) + geom_bar(fill = thecolor)
        }
        ########################################################

        ################ Scatterplot ##################
        if (viz == "scatter")
        {
            x <- isolate(input$xvar)
            y <- isolate(input$yvar)
            regression <- isolate(input$bestfit)
            if (regression == TRUE) 
            {
                p <- ggplot(finaldata, aes_string(x = x, y = y)) + geom_point() + geom_smooth(method = "lm")
            }
            else 
            {
                p <- ggplot(finaldata, aes_string(x = x, y = y)) + geom_point()

            }
        }
        ########################################################

            
  
        print(p)
      })

})  
}
