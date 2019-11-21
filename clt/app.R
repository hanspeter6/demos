#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(gridExtra)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("The Central Limit Theorem"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            selectInput("distribution", h3("Distribution:"),
                        c("Uniform (min = 0, max = 1)" = "uniform",
                          "Exponential (rate = 1)" = "exponential",
                          "Gamma (shape = 1.5)" = "gamma",
                        "Chi-Squared (df = 4)" = "chisquared")),
            hr(),
            
            sliderInput("n",
                        h3("Sample Size:"),
                        min = 1,
                        max = 30,
                        value = 5),
            hr(),
            
            actionButton("do", "Re Sample"),
            
            hr(),
            
            textOutput("sdeviations")
        ),
        
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    


    output$distPlot <- renderPlot({
    
        input$do
        
        if(input$distribution == "uniform") {
            
            averages <- data.frame(x = apply(matrix(runif(input$n*1000),nrow = input$n),2,mean))
            sds <- data.frame(s = apply(matrix(runif(input$n*1000),nrow = input$n),2,sd))
            underlying <- data.frame(u = runif(100000) )
            
            p <- ggplot(underlying, aes(x = u)) + 
                geom_histogram(aes(y = ..density..), colour = "white", fill = "grey", size = 0.1, bins = 50) +
                stat_function(
                        fun = dunif,
                    args = list(min = 0, max = 1)
                ) +
                scale_x_continuous("X")
            
            output$sdeviations <- renderText({
                
                paste("sigma = ", round(sd(underlying$u), 4), " ; s = ", round(mean(sds$s), 4))
                
            })
        
        }
        
        if(input$distribution == "exponential") {
            
            averages <- data.frame(x = apply(matrix(rexp(input$n*1000),nrow = input$n),2,mean))
            sds <- data.frame(s = apply(matrix(rexp(input$n*1000),nrow = input$n),2,sd))
            underlying <- data.frame(u = rexp(100000, rate = 1) )
            
            p <- ggplot(underlying, aes(x = u)) + 
                geom_histogram(aes(y = ..density..), colour = "white", fill = "grey", size = 0.1, bins = 50) +
                stat_function(
                    fun = dexp,
                    args = list(rate = 1)
                ) +
                scale_x_continuous("X")
            
            output$sdeviations <- renderText({
                
                paste("sigma = ", round(sd(underlying$u), 4), " ; s = ", round(mean(sds$s), 4))
                
            })
            
        }
        
        if(input$distribution == "gamma") {
            
            averages <- data.frame(x = apply(matrix(rgamma(input$n*1000, shape = 1.5),nrow = input$n),2,mean))
            sds <- data.frame(s = apply(matrix(rgamma(input$n*1000, shape = 1.5),nrow = input$n),2,sd))
            underlying <- data.frame(u = rgamma(100000, shape = 1.5) )
            
            p <- ggplot(underlying, aes(x = u)) + 
                geom_histogram(aes(y = ..density..), colour = "white", fill = "grey", size = 0.1, bins = 50) +
                stat_function(
                    fun = dgamma,
                    args = list(shape = 1.5)
                ) +
                scale_x_continuous("X")
            
            output$sdeviations <- renderText({
                
                paste("sigma = ", round(sd(underlying$u), 4), " ; s = ", round(mean(sds$s), 4))
                
            })
            
        }
        
        if(input$distribution == "chisquared") {
            
            averages <- data.frame(x = apply(matrix(rchisq(input$n*1000, df = 4),nrow = input$n),2,mean))
            sds <- data.frame(s = apply(matrix(rchisq(input$n*1000, df = 4),nrow = input$n),2,sd))
            underlying <- data.frame(u = rchisq(100000, df = 4) )
            
            p <- ggplot(underlying, aes(x = u)) + 
                geom_histogram(aes(y = ..density..), colour = "white", fill = "grey", size = 0.1, bins = 50) +
                stat_function(
                    fun = dchisq,
                    args = list(df = 4)
                ) +
                scale_x_continuous("X")
            
            output$sdeviations <- renderText({
                
                paste("sigma = ", round(sd(underlying$u), 4), " ; s = ", round(mean(sds$s), 4))
                
            })
            
        }

        # g <- ggplot(averages, aes(x = x)) +
        #     geom_histogram(aes(y = ..density..), colour = "white", fill = "cornflowerblue", size = 0.1, bins = 50) +
        #     stat_function(
        #         fun = dnorm,
        #         args = with(averages, c(mean = mean(x), sd = sd(x))),
        #         size = 1.2,
        #         col = "orange"
        #     ) +
        #     scale_x_continuous("X-BAR")
        
        g <- ggplot(averages, aes(x = x)) +
            geom_histogram(aes(y = ..density..), colour = "white", fill = "grey", size = 0.1, bins = 50, alpha = 0.4) +
            geom_density(aes(y = ..density..), colour = "white", fill = "cornflowerblue", size = 0.1, alpha = 0.2) +
            stat_function(
                fun = dnorm,
                args = with(averages, c(mean = mean(x), sd = sd(x))),
                size = 1.2,
                col = "orange"
            ) +
            scale_x_continuous("X-BAR")
        
        
        # p <- ggplot(underlying, aes(x = u)) + 
        #     geom_histogram(aes(y = ..density..), colour = "white", fill = "cornflowerblue", size = 0.1, bins = 100) +
        #     stat_function(
        #         if()
        #         fun = dnorm,
        #         args = with(averages, c(mean = mean(x), sd = sd(x)))
        #     ) +
        #     scale_x_continuous("X")
        
        grid.arrange(p,g,nrow = 1)
        
        
       
        
        })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
