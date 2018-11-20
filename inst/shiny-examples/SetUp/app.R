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
                   max = 20,
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
  
  maa <- reactive({
    mtemp <- matrix(input$Mbase, nrow=input$nyears, ncol=input$nages)
    if(input$Merrorflag == TRUE){
      mtemp <- addLognormalError(mtemp, input$Msigma, biasadjustflag = FALSE, randomval = NULL)
    }
    mtemp
  })
  
  output$dimPlot <- renderPlot({
    ya <- expand.grid(Age = ages(), Year = years())
    plot(ya$Age, ya$Year, xlab="Age", ylab="Year")
     title(paste("Number of Indices =", input$nindices))
   })
   
  output$Mplot <- renderPlot({
    matplot(maa())
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

