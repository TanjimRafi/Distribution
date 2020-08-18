library(shiny)

pageWithSidebar(
        
        # Title ----
        headerPanel("Distribution Visualization"),
        
        # Sidebar ----
        sidebarPanel(
                selectInput(
                        inputId = "dist",
                        label = "Distribution Name:",
                        choices = c("Binomial"     = "rbinom",
                                    "Beta"         = "rbeta",
                                    "Chi-Squared"  = "rchisq",
                                    "Cauchy"       = "rcauchy",
                                    "Exponential"  = "rexp",
                                    "F"            = "rf",
                                    "Gamma"        = "rgamma",
                                    "Geometric"    = "rgeom",
                                    "Normal"       = "rnorm",
                                    "Poisson"      = "rpois",
                                    "Student's t"  = "rt",
                                    "Uniform"      = "runif",
                                    "Weibull"      = "rweibull")
                        
                ),
                br(), 
                
                uiOutput("mean"),
                uiOutput("sd"),
                uiOutput("df1"),
                uiOutput("df2"),
                uiOutput("n"),
                uiOutput("p"),
                uiOutput("scale"),
                uiOutput("shape1"),
                uiOutput("shape2"),
                uiOutput("rate"),
                uiOutput("lambda"),
                uiOutput("min"),
                uiOutput("max"),
                
                helpText(a(href = "https://www.facebook.com/rafi.tanjim/", target = "_blank", "Tanjim Rafi")),
                helpText(a(href = "http://www.ru.ac.bd/stat/", target = "_blank", "Department of Statistics, University of Rajshahi")),
                helpText(a(href = "https://github.com/TanjimRafi/Distribution", target = "_blank", "View Code!!")),
                
        ),
        
        mainPanel(
                plotOutput("myplot")
        )
)