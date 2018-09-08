library(shiny)
library(colourpicker)
library(tidyverse)
library(patchwork)

server <- function(input, output) {


    ### Set ggplot theme ###
    theme_set(theme_classic() + theme(axis.text = element_text(size=15), 
                                      axis.title = element_text(size=18), 
                                      legend.text = element_text(size = 15),
                                      legend.title = element_text(size=17),
                                      legend.key.size = unit(1, "cm"),
                                      plot.margin = margin(t = 1, r = 1, b = 0.5, l = 0.5, unit = "cm")))    
                                      
    ### Reactive function to read in user data ###
    userdata <- reactive({
      infile <- input$datafile
      if (is.null(infile)) {
        # User has not uploaded a file yet
        return(NULL)
      }
      read_csv(infile$datapath)
    })

    ### Reactive variables for conditionalPanel interaction in ui.R
    output$userdata_exists <- reactive(!is.null(userdata()))
    outputOptions(output, "userdata_exists", suspendWhenHidden = FALSE)
    
    output$variables_selected <- reactive(!is.null(output$selectvars()))
    outputOptions(output, "variables_selected", suspendWhenHidden = FALSE)


    ### UI: Which type of quantitative plot?
    output$selectplot <- renderUI({
        
        if (is.null(userdata())) return(NULL)

        switch(input$dataviz, 
                "quant" = selectInput("whichquant", 
                        "Which type of plot would you like to make?", 
                        choices = c("Histogram", "Density plot", "Boxplot", "Jitter plot", "Make all")),
                "multquant" = selectInput("whichquant", 
                        "Which type of plot would you like to make?", 
                        choices = c("Boxplot", "Density plot", "Jitter plot", "Make all"))
                )
        ####### to do: we should be able to do this with barplots and error bars ########    
    })

    

    ### UI: Which variables to plot?
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
 
 
 
    ### UI: Summary table of *full* upload
    output$summary <- renderPrint({
        req(userdata())
        if (is.null(userdata())) return(NULL)
         summary(userdata())
    })


    ### "Regular" function which returns the plot
    zeplot <- function() {

        finaldata <- isolate(userdata()) 
        viz <- isolate(input$dataviz)
        geom_quant <- isolate(input$whichquant)
        thecolor <- isolate(input$yaycolor)


        ################ Single distribution ##################
        if (viz == "quant" & geom_quant == "Histogram")
        {
            quantvar2 <- isolate(input$quantvar)
            p <- ggplot(finaldata, aes_string(x = quantvar2)) + geom_histogram(color = "black", fill = thecolor)
        }
        if (viz == "quant" & geom_quant == "Density plot")
        {
            quantvar2 <- isolate(input$quantvar)
            p <- ggplot(finaldata, aes_string(x = quantvar2)) + geom_density(color = "black", fill = thecolor, alpha=0.8)
        }
        if (viz == "quant" & geom_quant == "Boxplot")
        {
            quantvar2 <- isolate(input$quantvar)
            p <- ggplot(finaldata, aes_string(y = quantvar2)) + geom_boxplot(fill = thecolor) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
        }
        if (viz == "quant" & geom_quant == "Jitter plot")
        {
            quantvar2 <- isolate(input$quantvar)
            p <- ggplot(finaldata, aes_string(y = quantvar2)) + geom_jitter(aes(x=""), color = thecolor, width=0.075) + xlab("")
        }
        if (viz == "quant" & geom_quant == "Make all")
        {
            quantvar2 <- isolate(input$quantvar)
            p1 <- ggplot(finaldata, aes_string(x = quantvar2)) + geom_histogram(color = "black", fill = thecolor)
            p2 <- ggplot(finaldata, aes_string(x = quantvar2)) + geom_density(color = "black", fill = thecolor, alpha=0.8)
            p3 <- ggplot(finaldata, aes_string(y = quantvar2)) + geom_boxplot(fill = thecolor)
            p4 <- ggplot(finaldata, aes_string(y = quantvar2)) + geom_jitter(aes(x=""), color = thecolor, width=0.075) + xlab("")
            p <- (p1 + p2) / (p3 + p4)
        }
        ########################################################

        ################ Multiple distributions ##################
        if (viz == "multquant" & geom_quant == "Boxplot")
        {
            quantvar2 <- isolate(input$quantvar)
            quantvar_cat2 <- isolate(input$quantvar_cat)

            #finaldata[,quantvar_cat2] <- factor(finaldata[,quantvar_cat2]) # doesn't work, why?!
            p <- ggplot(finaldata, aes_string(x = quantvar_cat2, y = quantvar2, group = quantvar_cat2, fill = paste0("factor(",quantvar_cat2,")"))) + geom_boxplot() + scale_fill_hue(name = quantvar_cat2, l=45)
        }
        if (viz == "multquant" & geom_quant == "Density plot")
        {
            quantvar2 <- isolate(input$quantvar)
            quantvar_cat2 <- isolate(input$quantvar_cat)
            p <- ggplot(finaldata, aes_string(x = quantvar2, fill = paste0("factor(",quantvar_cat2,")"))) + geom_density(color = "black", alpha=0.5) + scale_fill_hue(name = quantvar_cat2, l=45)
        }
        if (viz == "multquant" & geom_quant == "Jitter plot") ## todo: color by group such that it forces factor in aes_string?
        {
            quantvar2 <- isolate(input$quantvar)
            quantvar_cat2 <- isolate(input$quantvar_cat)
            p <- ggplot(finaldata, aes_string(x = quantvar_cat2, y = quantvar2, group = quantvar_cat2, color = paste0("factor(",quantvar_cat2,")") )) + geom_jitter(width = 0.1) + theme(legend.position = "none") + scale_fill_hue(name = quantvar_cat2, l=45)
        }
        if (viz == "multquant" & geom_quant == "Make all") 
        {
            quantvar2 <- isolate(input$quantvar)
            quantvar_cat2 <- isolate(input$quantvar_cat)
            p1 <- ggplot(finaldata, aes_string(x = quantvar_cat2, y = quantvar2, fill = paste0("factor(",quantvar_cat2,")"), group = quantvar_cat2)) + geom_boxplot() + theme(legend.position = "none") + scale_fill_hue(l=45)
            p2 <- ggplot(finaldata, aes_string(x = quantvar2, fill = paste0("factor(",quantvar_cat2,")") )) + geom_density(color = "black", alpha=0.6) + theme(legend.position = "bottom") + scale_fill_hue(l=45, name = quantvar_cat2)
            p3 <- ggplot(finaldata, aes_string(x = quantvar_cat2, y = quantvar2, group = quantvar_cat2, color = paste0("factor(",quantvar_cat2,")") )) + geom_jitter(width = 0.1) + theme(legend.position = "none") + scale_color_hue(l=45)
            p <- p1 + p2 + p3
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
                p <- ggplot(finaldata, aes_string(x = x, y = y)) + geom_point() + geom_smooth(color = thecolor, method = "lm")
            }
            else 
            {
                p <- ggplot(finaldata, aes_string(x = x, y = y)) + geom_point()

            }
        }
        ########################################################
        print(p)
    }


    ### Render plot and download button, reactive to "Go" button
    observeEvent(input$go,  {

    
        output$mahplot <- renderPlot( { 
            zeplot()
        })

        output$download <- renderUI({
            downloadButton('download_plot', 'Download plot')
        })


       output$download_plot <- downloadHandler(
            filename = function() {
                "download.png"
            },
            content = function(file) {
                ggsave(file, zeplot(), width = 6, height = 4)
            }
        )
    })  
}

