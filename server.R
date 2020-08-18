library(shiny)

function(input , output){

        # Normal distribution #
        
        output$mean <- renderUI({
                # print("mean")
                if (input$dist %in% c("rnorm" ,"rcauchy")) {
                        numericInput("mu",
                                     "Mean/Location"
                                     , value = 0
                        )
                }
        })
        
        output$sd <- renderUI({
                # print("sd")
                if (input$dist == "rnorm") {
                        numericInput("sd",
                                     "Standard deviation",
                                     value = 1,
                                     min = 0,
                                     step = 0.1
                        )
                }
        })
        
        # t, F, X^2 distribution #
        
        output$df1 <- renderUI({
                # print("df1")
                if (input$dist %in% c("rt", "rchisq", "rf")) {
                        numericInput(ifelse(input$dist %in% c("rt", "rchisq"), "df", "df1"),
                                     "Degrees of freedom",
                                     value = 1,
                                     min = 1,
                                     step = 1
                        )
                }
        })
        
        output$df2 <- renderUI({
                # print("df2")
                if (input$dist == "rf") {
                        numericInput("df2",
                                     "Degrees of freedom(2)",
                                     value = 10,
                                     min = 1,
                                     step = 1
                        )
                }
        })
        
        # Binomial distribution #

        output$n <- renderUI({
                # print("n")
                if (input$dist == "rbinom") {
                        numericInput("n",
                                     "Number of trials:",
                                     value = 10,
                                     min = 1,
                                     step = 1
                        )
                }
        })
        
        output$p <- renderUI({
                # print("p")
                if (input$dist %in% c("rbinom","rgeom")) {
                        sliderInput("p",
                                    "Probability of success",
                                    value = 0.5,
                                    min = 0,
                                    max = 1,
                                    step = .01
                        )
                }
        })
        
        # Cauchy and Weibull distribution #
        
        output$scale <- renderUI({
                if(input$dist %in% c("rcauchy","rweibull")){
                        numericInput("scale"
                                     , "Scale:"
                                     , min = 1
                                     , max = 50
                                     , value = 1)
                }
        })
        
        # Beta , Gamma and Weibull distribution #
        
        output$shape1 <- renderUI({
                if(input$dist %in% c("rgamma","rweibull","rbeta")){
                        numericInput(ifelse(input$dist %in% c("rweibull","rgamma") , "shape" , "shape1")
                                     , "Shape(alpha):"
                                     , min = 0
                                     , value = 1)
                }
        })
        
        output$shape2 <- renderUI({
                if(input$dist %in% "rbeta"){
                        numericInput("shape2"
                                     , "Shape2(beta):"
                                     , min = 0
                                     , value = 1)
                }
        })

        # Exponential and Gamma distribution #
        
        output$rate <- renderUI({
                if(input$dist %in% c("rgamma","rexp")){
                        numericInput("rate"
                                     , "Rate(1/Scale):"
                                     , min = 0 
                                     , value = 0.5
                                     , step = 0.01)
                }
        })

        # Poisson distribution #
        
        output$lambda <- renderUI({
                if(input$dist == "rpois"){
                        numericInput("lambda"
                                     , "Lambda"
                                     , min = 0
                                     , value = 1
                                     , step = 1)
                        
                }
        })

        # Uniform distribution #
        
        output$min <- renderUI({
                if(input$dist == "runif"){
                        numericInput("min"
                                     , "Minimum:"
                                     , min = 0
                                     , value = 1
                                     , step = 1)
                }
        })
        
        output$max <- renderUI({
                if(input$dist == "runif"){
                        numericInput("max"
                                     , "Maximum:"
                                     , min = 0
                                     , value = 5
                                     , step = 1)
                }
        })

        # Plot #
        
        output$myplot <- renderPlot({
                
                if(input$dist == "rnorm"){
                        l <- input$mu - 10
                        u <- input$mu + 10
                        x <- seq(l , u , 0.01)
                        fx <- dnorm(x , mean = input$mu , sd =input$sd)
                        plot(x , fx , type = "l" , ylab = "Probability density function" , ylim = c(0,0.4) 
                             , main = paste("Mean =",round(input$mu , 2)
                                            , " and Standard deviation =",round(input$sd, 2)))
                }else if(input$dist == "rt"){
                        x <- seq(-5 , 5 , 0.01)
                        fx <- dt(x , df = input$df)
                        plot(x , fx , type = "l" , ylim = c(0 , 0.4) 
                             , ylab = "Probability density function" , lwd = 2
                             , main = paste("Mean =", "0 for df > 1 , otherwise Undefined"
                                            , " and Standard deviation =",round(sqrt(input$df/(input$df - 2)), 2)))
                }
                else if(input$dist == "rchisq"){
                        l <- input$df - 20
                        u <- input$df + 20
                        x <- seq(l , u , 0.01)
                        fx <- dchisq(x , df = input$df)
                        plot(x , fx , type = "l" , ylim = c(0,0.5)
                             , xlab = "Degree of freedom" , ylab = "Probability density function"
                             , main = paste("Mean =",input$df
                                            , " and Standard deviation =",round(sqrt(2*input$df),2)))
                }
                else if(input$dist == "rf"){
                        l <- round(input$df2/(input$df2 - 2),2) - 5
                        u <- round(input$df2/(input$df2 - 2),2) + 10
                        x <- seq(l , u , 0.01)
                        fx <- df(x ,df1 = input$df1 , df2 = input$df2)
                        plot(x , fx , type = "l"
                             , xlab = "Degree of freedom" , ylab = "Probability density function" , lwd = 2
                             , main = paste("Mean =",round(input$df2/(input$df2 - 2),2)
                                            , " and Standard deviation =",round(sqrt((2*input$df2^2*(input$df2 + input$df2 -2))/((input$df1*(input$df2-2)^2)*(input$df2-4))),2)))
                        
                }
                else if(input$dist == "rbinom"){
                        l <- round(input$n*input$p) - 20
                        u <- round(input$n*input$p) + 20
                        x <- seq(l , u , 1)
                        fx <- dbinom(x , size = input$n , prob = input$p)
                        plot(x , fx , type = "h" , ylim = c(0,0.3) , lwd = 2
                             , xlab = "Probability" , ylab = "Probability mass function" 
                             , main = paste("Mean =",input$n*input$p 
                                            , " and Standard deviation =",round(sqrt(input$n*(1-input$p)), 2)))
                        
                }
                else if(input$dist == "rcauchy"){
                        l <- input$mu - 50
                        u <- input$mu + 50
                        x <- seq(l , u , 0.01)
                        fx <- dcauchy(x , location = input$mu , scale = input$scale)
                        plot(x , fx , type = "l" , ylab = "Probability density function" , ylim = c(0,0.3) , lwd = 2
                             , main = "The standard Cauchy distribution coincides with the Student's t distribution with 1 degree of freedom")
                        
                }
                else if(input$dist == "rbeta"){
                        l <- round(input$shape1/(input$shape1 + input$shape2),2) - 2
                        u <- round(input$shape1/(input$shape1 + input$shape2),2) + 5
                        x <- seq(l , u , 0.01)
                        fx <- dbeta(x ,shape1 = input$shape1 ,shape2 = input$shape2)
                        plot(x , fx , type = "l", xlab = "Probability" , ylab = "Probability density function" , lwd = 2
                             , main = paste("Mean =",round(input$shape1/(input$shape1 + input$shape2),2)
                                            , " and Standard deviation =",round(sqrt(input$shape1*input$shape2/((input$shape1 + input$shape2)^2/(input$shape1 + input$shape2 + 1))), 2)))
                }
                else if(input$dist == "rexp"){
                        l <- round(1/input$rate , 2) - 5
                        u <- round(1/input$rate , 2) + 10
                        x <- seq(l , u , 0.01) 
                        fx <- dexp(x , rate = input$rate)
                        plot(x,fx, type = "l" , ylim = c(0,0.5) , ylab = "Probability density function" , lwd = 2
                             , main = paste("Mean =",round(1/input$rate , 2) 
                                            , " and Standard deviation =",round(sqrt(1/input$rate^2), 2)))
                }
                else if(input$dist == "rgamma"){
                        l <- round(input$shape/input$rate , 2) - 5
                        u <- round(input$shape/input$rate , 2) + 10
                        x <- seq(l , u , 0.01) 
                        fx <- dgamma(x ,shape = input$shape , rate = input$rate)
                        plot(x,fx, type = "l" , ylim = c(0,1) , ylab = "Probability" , lwd = 2
                             , main = paste("Mean =",round(input$shape/input$rate , 2)
                                            , " and Standard deviation =",round(sqrt(input$shape/input$rate^2), 2)))
                }
                else if(input$dist == "rgeom"){
                        l <- round(1/input$p , 2) - 5
                        u <- round(1/input$p , 2) + 10
                        x <- seq(l , u , 0.01) 
                        fx <- dgeom(x , prob = input$p)
                        plot(x,fx, type = "h" , xlab = "Trials needed to get the first success"
                             , ylab = "Probability" , lwd = 2
                             , main = paste("Mean =",round(1/input$p , 2)
                                            , " and Standard deviation =",round(sqrt((1-input$p)/input$p^2), 2)))
                }
                else if(input$dist == "rpois"){
                        l <- round(input$lambda , 2) - 20
                        u <- round(input$lambda , 2) + 20
                        x <- seq(l , u , 1) 
                        fx <- dpois(x , lambda = input$lambda)
                        plot(x,fx, type = "h" , xlab = "Number of events per unit of time"
                             , ylab = "Probability" , ylim = c(0,0.3) , lwd = 2
                             , main = paste("Mean = Variance =",round(input$lambda , 2)))
                        
                }
                else if(input$dist == "runif"){
                        l <- round(0.5*(input$min + input$max) , 2) - 10
                        u <- round(0.5*(input$min + input$max) , 2) + 10
                        x <- seq(l , u , 0.01) 
                        fx <- dunif(x , min = input$min , max = input$max)
                        plot(x,fx, type = "l" , xlab = "X"
                             , ylab = "Probability" , ylim = c(0,1) , lwd = 2
                             , main = paste("Mean =",round(0.5*(input$min + input$max) , 2)
                                            , " and Standard deviation =",round(sqrt((1/12)*(input$max - input$min)^2), 2)))
                }
                else{
                        l <- round(input$scale*gamma(1 + (1/input$shape)) , 2) - 5
                        u <- round(input$scale*gamma(1 + (1/input$shape)) , 2) + 10
                        x <- seq(l , u , 0.01)
                        fx <- dweibull(x , shape = input$shape , scale = input$scale)
                        plot(x , fx , type = "l" , ylim = c(0,2) 
                             , ylab = "Probability density function" , lwd = 2
                             , main =  paste("Mean =",round(input$scale*gamma(1 + (1/input$shape)) , 2)
                                             , " and Standard deviation =",round(sqrt(input$scale^2*((gamma(1+2/input$shape)) - (gamma(1 + 1/input$shape))^2)), 2)))
                }
        })
}
