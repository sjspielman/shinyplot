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
                        choices = c("Histogram", "Density plot", "Boxplot", "Jitter plot", "Bar plot", "Make all")),
                "multquant" = selectInput("whichquant", 
                        "Which type of plot would you like to make?", 
                        choices = c("Boxplot", "Density plot", "Jitter plot", "Bar plot", "Line plot", "Make all"))
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




    ################################################################################################
    ############################### Functions for making individual plots ##########################
    isolate_data <- function(){
        isolate(userdata()) 
    }        


    isolate_color <- function(){
        isolate(input$yaycolor)
    }
        plot_histogram <- function()
    {
        finaldata <- isolate_data()
        thecolor  <- isolate_color()
        quantvar2 <- as.symbol( isolate(input$quantvar) )
        p <- ggplot(finaldata, aes(x = !!quantvar2)) + geom_histogram(color = "black", fill = thecolor)
        p    
    }

    plot_density <- function()
    {
        finaldata <- isolate_data()
        thecolor  <- isolate_color()
        quantvar2 <- as.symbol( isolate(input$quantvar) )
        p <- ggplot(finaldata, aes(x = !!quantvar2)) + geom_density(color = "black", fill = thecolor, alpha=0.8)
        p    
    }
    plot_boxplot <- function()
    {
        finaldata <- isolate_data()
        thecolor  <- isolate_color()
        quantvar2 <- as.symbol( isolate(input$quantvar) )
        p <- ggplot(finaldata, aes(y = !!quantvar2)) + geom_boxplot(fill = thecolor) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
        p    
    }

    plot_jitter <- function()
    {
        finaldata <- isolate_data()
        thecolor  <- isolate_color()
        quantvar2 <- as.symbol( isolate(input$quantvar) )
        p <- ggplot(finaldata, aes(y = !!quantvar2)) + geom_jitter(aes(x=""), color = thecolor, width=0.075) + xlab("")
        p    
    }
    plot_barquant <- function()
    {
        finaldata <- isolate_data()
        thecolor  <- isolate_color()
        quantvar2 <- as.symbol( isolate(input$quantvar) )
        p <- ggplot(finaldata, aes(x="")) + stat_summary(aes(y = !!quantvar2), fun.y = "mean", geom = "bar", width=0.1, fill = thecolor) + stat_summary(aes(y = !!quantvar2), fun.dat = "mean_se", geom = "errorbar", width = 0.05) + xlab("")
        p    
    }

    plot_multbox <- function()
    {
        finaldata <- isolate_data()
        quantvar2 <-  as.symbol(isolate(input$quantvar))
        quantvar_cat2 <-  as.symbol(isolate(input$quantvar_cat))
        p <- ggplot(finaldata, aes(x = !!quantvar_cat2, y = !!quantvar2, group = !!quantvar_cat2, fill = factor(!!quantvar_cat2))) + geom_boxplot() + scale_fill_hue(name = quantvar_cat2, l=45)
        p
    }
    plot_multdensity <- function()
    {
        finaldata <- isolate_data()
        quantvar2 <-  as.symbol(isolate(input$quantvar))
        quantvar_cat2 <-  as.symbol(isolate(input$quantvar_cat))
        p <- ggplot(finaldata, aes(x = !!quantvar2, fill = factor(!!quantvar_cat2))) + geom_density(color = "black", alpha=0.5) + scale_fill_hue(name = quantvar_cat2, l=45)
        p
    }
    plot_multjitter <- function()
    {
        finaldata <- isolate_data()
        quantvar2 <-  as.symbol(isolate(input$quantvar))
        quantvar_cat2 <-  as.symbol(isolate(input$quantvar_cat))
        p <- ggplot(finaldata, aes(x = !!quantvar_cat2, y = !!quantvar2, group = !!quantvar_cat2, color = factor(!!quantvar_cat2))) + geom_jitter(width = 0.1) + scale_color_hue(name = quantvar_cat2, l=45)
        p
    }
    plot_multbarquant <- function()
    {
        finaldata <- isolate_data()
        quantvar2 <-  as.symbol(isolate(input$quantvar))
        quantvar_cat2 <-  as.symbol(isolate(input$quantvar_cat))
        p <- ggplot(finaldata) + stat_summary_bin(aes(x = factor(!!quantvar_cat2), y = !!quantvar2, fill = factor(!!quantvar_cat2)), fun.y = "mean", geom = "bar", width=0.1) + stat_summary_bin(aes(factor(!!quantvar_cat2), y = !!quantvar2), fun.data = "mean_se", geom = "linerange", size=1.5) + xlab(quantvar_cat2) +  scale_fill_hue(name = quantvar_cat2, l=45)
        p
    }
    plot_line <- function()
    {
        finaldata <- isolate_data()
        quantvar2 <-  as.symbol(isolate(input$quantvar))
        quantvar_cat2 <-  as.symbol(isolate(input$quantvar_cat))
        p <- ggplot(finaldata) + 
                stat_summary_bin(aes(x = factor(!!quantvar_cat2), y = !!quantvar2, color = factor(!!quantvar_cat2)), fun.y = "mean", geom = "point", size=5) +
                stat_summary_bin(aes(x = factor(!!quantvar_cat2), y = !!quantvar2), group=1, fun.y = "mean", geom = "line") +
                stat_summary_bin(aes(factor(!!quantvar_cat2), y = !!quantvar2), fun.data = "mean_se", geom = "linerange") + xlab(quantvar_cat2) +  scale_color_hue(name = quantvar_cat2, l=45) 
        p
    }

    plot_barcount <- function()
    {
        finaldata <- isolate_data()
        thecolor  <- isolate_color()
        countvar <-  as.symbol(isolate(input$countvar))
        p <- ggplot(finaldata, aes(x = factor(!!countvar))) + geom_bar(fill = thecolor) + xlab(countvar)
        p
    }
    
    plot_scatter <- function()
    {
        finaldata <- isolate_data()
        thecolor  <- isolate_color()
        x <-  as.symbol(isolate(input$xvar))
        y <-  as.symbol(isolate(input$yvar))
        regression <- isolate(input$bestfit)
        p <- ggplot(finaldata, aes(x = !!x, y = !!y)) + geom_point()
        if (regression == TRUE) 
        {
            p <- p + geom_smooth(color = thecolor, method = "lm")
        }
        p
    }
    ################################################################################################
    ################################################################################################
    

    
    ### "Regular" function which returns the plot
    printplot <- function() {

        viz <- isolate(input$dataviz)
        geom_quant <- isolate(input$whichquant)

        ################ Single distribution ##################
        if (viz == "quant" & geom_quant == "Histogram") p <- plot_histogram()

        if (viz == "quant" & geom_quant == "Density plot") p <- plot_density()

        if (viz == "quant" & geom_quant == "Boxplot") p <- plot_boxplot()

        if (viz == "quant" & geom_quant == "Jitter plot") p <- plot_jitter()

        if (viz == "quant" & geom_quant == "Bar plot") p <- plot_barquant()

        if (viz == "quant" & geom_quant == "Make all")
        {
            p1 <- plot_histogram()
            p2 <- plot_density()
            p3 <- plot_boxplot()
            p4 <- plot_jitter()
            p5 <- plot_barquant()
            
            p <- (p1 + p2 + p3) / (p4 + p5)
        }
        ########################################################

        ################ Multiple distributions ##################
        if (viz == "multquant" & geom_quant == "Boxplot") p <- plot_multbox()

        if (viz == "multquant" & geom_quant == "Density plot") p <- plot_multdensity()

        if (viz == "multquant" & geom_quant == "Jitter plot") p <- plot_multjitter()

        if (viz == "multquant" & geom_quant == "Bar plot") p <- plot_multbarquant()

        if (viz == "multquant" & geom_quant == "Line plot") p <- plot_line()

        if (viz == "multquant" & geom_quant == "Make all")
        {
            p1 <- plot_multbox() + theme(legend.position = "none")
            p2 <- plot_multdensity() + theme(legend.position = "none")
            p3 <- plot_multjitter() + theme(legend.position = "none")
            p4 <- plot_multbarquant() + theme(legend.position = "bottom")
            p5 <- plot_line() + theme(legend.position = "none")
            
            p <- (p1+p2+p3)/(p4+p5)
        }
        ########################################################


        ################ Barplot for counts ####################
        if (viz == "counts") p <- plot_barcount()
        ########################################################

        ################ Scatterplot ###########################
        if (viz == "scatter") p <- plot_scatter()
        ########################################################
        
        print(p)
    }


    ### Render plot and download button, reactive to "Go" button
    observeEvent(input$go,  {

    
        output$mahplot <- renderPlot( { 
            printplot()
        })

        output$download <- renderUI({
            downloadButton('download_plot', 'Download plot')
        })


       output$download_plot <- downloadHandler(
            filename = function() {
                "download.png"
            },
            content = function(file) {
                ggsave(file, printplot(), width = 6, height = 4)
            }
        )
    })  
}

