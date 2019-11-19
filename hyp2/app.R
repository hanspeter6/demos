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
library(plotly)
library(grid)
library(gridExtra)

full <- function() {
    print(g)
    theme_set(theme_bw(base_size = 8))
    print(q, vp = vp)
    theme_set(theme_bw())
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        h4("Select Sample Size:"),
                        min = 1,
                        max = 30,
                        value = 5),
            hr(),
            
            sliderInput("mu",
                        h4("Select Hypothesised Mean:"),
                        min = 60,
                        max = 80,
                        value = 70),
            hr(),
            
            sliderInput(inputId = "alpha:",
                        label = h4("Select alpha:"),
                        value = 0.05,
                        min = 0.001,
                        max = 0.1),
            hr()
            

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
        
        # two-tailed
        
        # starts with a claim:
        # Example: The population average weight is 65kg; or
        # Example: The population average weight is not 65kg
        
        # convention: the null hypothesis always has the equal to 
        # H0: mu = 65
        # H1: mu != 65
        
        mu_0 <- input$mu # user input
        
        # Decide on an acceptable type one error: the probability of rejecting a true hypothesis
        
        alph <- input$alpha
        
        # pick a sample (should be random - difficult to do...)
        # decide on the sample size (n)
        
        n <- input$n  #user input n
        
        # weigh them and calculate the statistics
        set.seed(3)
        mySample <- rnorm(n, mean = 73, sd = 3) # open a window to show this
        
        # mySample <- c(56,76,45,34,78)
        
        xbar <- mean(mySample) # display
        s <- sd(mySample) # display
        
        # this is my evidence: Given this evidence how likely or unlikely is my null hypothesis mu = 65
        
        
        # my sample provided evidence of a value to compare with this:
        t_calc <- (xbar - mu_0)/(s/sqrt(n))
        
        # # where is it?
        # points(x = t_calc, y = 0, col = "blue", pch = 24)
        
        # is that likely or unlikely... unlikely
        
        # two ways: critical value way:
        t_crit <- abs(qt(alph/2, df = n-1))
        
        g <- ggplot(data.frame(x = c(-5, 5)), aes(x = x)) +
            stat_function(fun = dt, args = list(df = n-1)) +
            stat_function(fun = dt, args = list(df = n-1), xlim = c(-5, -t_crit),
                          geom = "area", fill = "red", alpha = .6) +
            stat_function(fun = dt, args = list(df = n-1), xlim = c(t_crit, +5),
                          geom = "area", fill = "red", alpha = .6) +
            # geom_point(aes(x = t_calc, y = -0.01), col = "blue", shape = 17, size = 4) +
            geom_point(aes(x = t_crit, y = -0.01), col = "red", fill = "red", shape = 24, size = 3) +
            geom_point(aes(x = -t_crit, y = -0.01), col = "red", fill = "red", shape = 24, size = 3) +
            scale_x_continuous(name = "T") +
            scale_y_continuous(name = "density") +
            annotate(geom = "text", x = -t_crit, y = -0.03, label ="-t-crit", col = "brown", size = 3) +
            annotate(geom = "text", x = t_crit, y = -0.03, label ="+t-crit", col = "brown", size = 3) +
            annotate(geom = "text", x = t_calc, y = -0.05, label ="t-calc", size = 4) +
            geom_segment(aes(x = t_calc, y = -0.04, xend = t_calc, yend = 0), arrow = arrow()) +
            geom_hline(yintercept = 0, size = 0.4)
        
        q <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
            stat_function(fun = dt, args = list(df = n-1)) +
            stat_function(fun = dnorm, args = list(mean = 0, sd = 1), col = "red")
        
        grid.arrange(g,q, nrow = 1 )

        
        # vp <- viewport(width = 0.3, height = 0.3, x = 0.8, y = 0.8)
        # 
        # full()
        
        
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
