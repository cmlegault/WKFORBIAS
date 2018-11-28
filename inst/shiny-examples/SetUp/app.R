# This is a Shiny app to create a simple set up for use in WKFORBIAS. 
# To run it, use the command runShiny() in R. 
# Assumes shiny package is installed.

ui <- navbarPage(strong("WKFORBIAS Set Up"),
     
  tabPanel("Dimensions",
    sidebarLayout(
     sidebarPanel(
       selectInput("year1",
                   "First Year",
                   choices = c(1900:2020),
                   selected = 1991),
       
       sliderInput("nyears",
                   "Number of Years",
                   min = 5,
                   max = 100,
                   value = 20),
       
       sliderInput("nages",
                   "Number of Ages",
                   min = 4,
                   max = 50,
                   value = 10),
       
       checkboxInput("plusgroupflag",
                     "Last Age a Plus Group?",
                     value = TRUE),
       
       sliderInput("nindices",
                   "Number of Indices",
                   min = 1,
                   max = 4,
                   value = 2)
     ),
     mainPanel(
       plotOutput("dimPlot")
     )
    )
  ),
  
  tabPanel("M",
    sidebarLayout(
     sidebarPanel(
       selectInput("Mopt",
                 "Natural Mortality",
                 choices = list("Single Value", "Constant over Time", "More"),
                 selected = "Single Value"),
       
       sliderInput("Mbase",
                   "Base Natural Mortality Rate",
                   min = 0.01,
                   max = 0.90,
                   step = 0.01,
                   value = 0.2),
       
       checkboxInput("Merrorflag",
                     "Add variability to M matrix?",
                     value = FALSE),
       
       sliderInput("Msigma",
                   "Sigma for added error to M matrix",
                   min = 0,
                   max = 1,
                   step = 0.01,
                   value = 0)
     ),
     mainPanel(
       plotOutput("Mplot")
     )
    )
  ),
  
  tabPanel("F",
    sidebarLayout(
      sidebarPanel(
        sliderInput("Fbase",
                    "Base Fishing Mortality Rate",
                    min = 0,
                    max = 2,
                    step = 0.05,
                    value = 0.25),
        
        checkboxInput("Ferrorflag",
                      "Add variability to F matrix?",
                      value = FALSE),
               
        sliderInput("Fsigma",
                    "Sigma for added error to F matrix",
                    min = 0,
                    max = 1,
                    step = 0.01,
                    value = 0)
      ),
      mainPanel(
        plotOutput("Fplot")
      )
    )
  ),
  
  tabPanel("WAA",
    sidebarLayout(
      sidebarPanel(
        sliderInput("Winfyear1",
                    "Winfinity in first year",
                    min = 1,
                    max = 100,
                    step = 1,
                    value = 10),
        sliderInput("Kyear1",
                    "K in first year",
                    min = 0.1,
                    max = 0.9,
                    step = 0.05,
                    value = 0.3)
      ),
      mainPanel(
        plotOutput("Wplot")
      )
    )
  ),
  
  navbarMenu("Indices",
    tabPanel("Index 1",
      sidebarLayout(
        sidebarPanel(
          sliderInput("i1A50",
                      "Index 1 A50",
                      min = 0,
                      max = 10,
                      step = 0.1,
                      value = 4),
          
          sliderInput("i1slope",
                      "Index 1 slope",
                      min = -10,
                      max = 10,
                      step = 0.1,
                      value = 1)
        ),
        mainPanel(
          plotOutput("indexplot")
        )
      )
    )
  ),
  
  tabPanel("Nyear1",
    sidebarLayout(
     sidebarPanel(
       selectInput("Nyear1opt",
                   "Population numbers at age in first year",
                   choices = list("User Input", "Equilibrium", "Equilibrium with Noise"),
                   selected = "Equilibrium")
     ),
     mainPanel(
       plotOutput("Nyear1plot")
     )
    )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
     
  years <- reactive({
    yr1 <- as.numeric(input$year1)
    seq(yr1, yr1 + input$nyears - 1)
  })  
  
  ages <- reactive({
    seq(1, input$nages)
  })
  
  Mlist <- reactive({
    M <- list()
    M$base <- matrix(input$Mbase, nrow=input$nyears, ncol=input$nages)
    M$values <- M$base
    M$Merrorflag <- input$Merrorflag
    if(input$Merrorflag == TRUE){
      M$noise <- addLognormalError(M$base, input$Msigma, biasadjustflag = FALSE, randomval = NULL)
      M$values <- M$noise
    }
    M
  })
  
  Flist <- reactive({
    FAA <- list()
    FAA$base <- matrix(input$Fbase, nrow=input$nyears, ncol=input$nages)
    FAA$values <- FAA$base
    FAA$Ferrorflag <- input$Ferrorflag
    if(input$Ferrorflag == TRUE){
      FAA$noise <- addLognormalError(FAA$base, input$Fsigma, biasadjustflag = FALSE, randomval = NULL)
      FAA$values <- FAA$noise
    }
    FAA
  })
  
  Wlist <- reactive({
    W <- list()
    WAA <- input$Winfyear1 * (1 - exp(-input$Kyear1 * seq(1, input$nages))) ^ 3
    W$base <- matrix(rep(WAA, each=input$nyears), nrow=input$nyears, ncol=input$nages)
    W$values <- W$base
    W
  })
  
  indexlist <- reactive({
    index <- list()
    for (ind in 1:input$nindices){
      index[[ind]] <- list()
      if (ind == 1){
        index[[ind]]$A50 <- input$i1A50
        index[[ind]]$selx <- 1 / (1 + exp(-input$i1slope * (ages() - input$i1A50)))
      }
    }
    index
  })
  
  output$dimPlot <- renderPlot({
    ya <- expand.grid(Age = ages(), Year = years())
    plot(ya$Age, ya$Year, xlab="Age", ylab="Year")
     title(paste("Number of Indices =", input$nindices))
   })
   
  output$Mplot <- renderPlot({
    matplot(Mlist()$values)
  })
   
  output$Fplot <- renderPlot({
    matplot(Flist()$values)
  })
  
  output$Wplot <- renderPlot({
    matplot(Wlist()$values)
  })
  
  output$indexplot <- renderPlot({
    plot(ages(), indexlist()[[1]]$selx)
  })
  
  output$Nyear1plot <- renderPlot({
    Nyear1 <- rep(1000, input$nages)
    zaa <- 0.5
    for (iage in 2:input$nages){
      Nyear1[iage] <- Nyear1[iage - 1] * exp(-zaa)
    }
    if (input$plusgroupflag == TRUE){
      Nyear1[input$nages] <- Nyear1[input$nages] / (1 - exp(-zaa))
    }
    plot(1:input$nages, Nyear1, type='o')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

