# This is a Shiny app to create a simple set up for use in WKFORBIAS. 
# To run it, use the command runShiny() in R. 
# Assumes shiny package is installed.

# Shiny user interface
ui <- navbarPage(strong("WKFORBIAS Set Up"),
     
  tabPanel("Dimensions",
    sidebarLayout(
     sidebarPanel(
       selectInput("year1",
                   "First Year",
                   choices = c(1900:2020),
                   selected = 1991),
       
       sliderInput("nYear",
                   "Number of Years",
                   min = 5,
                   max = 100,
                   value = 20),
       
       sliderInput("nAge",
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
                 choices = list("Single Value", "Constant over Time", "Matrix"),
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
        sliderInput("Fyears",
                    "Years for Setting F (in addition to first and last year)",
                    min = 1,
                    max = 10,
                    step = 1,
                    value = c(4,5)),
        
        sliderInput("Fages",
                    "Ages for Setting F (in addition to first and last age)",
                    min = 1,
                    max = 10,
                    step = 1,
                    value = 3),
        
        fluidRow(
          column(4,
            sliderInput("Fy1a1", "Fy1a1",
                        min = 0, max = 1, step = 0.1, value = 0.1),
            sliderInput("Fy2a1", "Fy2a1",
                        min = 0, max = 1, step = 0.1, value = 0.1),
            sliderInput("Fy3a1", "Fy3a1",
                        min = 0, max = 1, step = 0.1, value = 0.1),
            sliderInput("Fy4a1", "Fy4a1",
                        min = 0, max = 1, step = 0.1, value = 0.1)),
          column(4,
            sliderInput("Fy1a2", "Fy1a2",
                        min = 0, max = 1, step = 0.1, value = 0.4),
            sliderInput("Fy2a2", "Fy2a2",
                        min = 0, max = 1, step = 0.1, value = 0.6),
            sliderInput("Fy3a2", "Fy3a2",
                        min = 0, max = 1, step = 0.1, value = 0.7),
            sliderInput("Fy4a2", "Fy4a2",
                        min = 0, max = 1, step = 0.1, value = 0.3)),
          column(4,
            sliderInput("Fy1a3", "Fy1a3",
                        min = 0, max = 1, step = 0.1, value = 0.4),
            sliderInput("Fy2a3", "Fy2a3",
                        min = 0, max = 1, step = 0.1, value = 0.6),
            sliderInput("Fy3a3", "Fy3a3",
                        min = 0, max = 1, step = 0.1, value = 0.5),
            sliderInput("Fy4a3", "Fy4a3",
                        min = 0, max = 1, step = 0.1, value = 0.2))
        )
      ),
      mainPanel(
        tableOutput("Ftable")
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
                      max = 50,
                      step = 0.1,
                      value = 4),
          
          sliderInput("i1slope",
                      "Index 1 slope",
                      min = -10,
                      max = 10,
                      step = 0.1,
                      value = 1),
          
          sliderInput("i1A502",
                      "Index 1 A50 2",
                      min = 0,
                      max = 50,
                      step = 0.1,
                      value = 10),
          
          sliderInput("i1slope2",
                      "Index 1 slope 2",
                      min = -10,
                      max = 10,
                      step = 0.1,
                      value = 0),
          
          sliderInput("i1q",
                      "Index 1 catchability",
                      min = 0.01,
                      max = 1,
                      step = 0.01,
                      value = 0.3)
        ),
        mainPanel(
          plotOutput("indexplot")
        )
      )
    )
  ),
  
  tabPanel("Recruits",
    sidebarLayout(
      sidebarPanel(
        sliderInput("R0",
                    "Unexploited recruitment (millions)",
                    min = 1,
                    max = 100,
                    step = 1,
                    value = 14),
      
        sliderInput("steepness",
                    "Steepness of Bev-Holt curve",
                    min = 0.21,
                    max = 1,
                    step = 0.01,
                    value = 0.7),
        
        sliderInput("Rsigma",
                    "Sigma for error about SR curve",
                    min = 0,
                    max = 2,
                    step = 0.1,
                    value = 0),
        
        checkboxInput("Rsigmabiasflag",
                      "Bias adjust recruitment values?",
                      value = FALSE)
      ),
      mainPanel(
        plotOutput("Recruitmentplot"),
        plotOutput("Nyear1plot")
      )
    )
  ),
  
  tabPanel("Download",
    sidebarLayout(
      sidebarPanel(
        downloadButton("downloadinput", "Download Shiny input"),
        downloadButton("downloadoutput", "Download Shiny output")
      ),
      mainPanel(
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observe({
    year1 <- as.numeric(input$year1)
    nYear <- input$nYear
    year2 <- year1 + nYear - 1
    updateSliderInput(session, "Fyears", 
                      value = c((year1 + floor(nYear / 3)), (year1 + ceiling(2 * nYear / 3))),
                      min = year1, max = year2, step = 1)
  })
  
  observe({
    nAge <- input$nAge
    updateSliderInput(session, "Fages", value = floor(nAge/2),
                      min = 1, max = nAge, step = 1)
  })
  
  years <- reactive({
    yr1 <- as.numeric(input$year1)
    seq(yr1, yr1 + input$nYear - 1)
  })  
  
  ages <- reactive({
    seq(1, input$nAge)
  })
  
  Mlist <- reactive({
    M <- list()
    M$base <- matrix(input$Mbase, nrow=input$nYear, ncol=input$nAge, dimnames=list(years()))
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
    year1 <- as.numeric(input$year1)
    nYear <- input$nYear
    nAge <- input$nAge
    y1 <- 1
    y2 <- input$Fyears[1] - year1 + 1
    y3 <- input$Fyears[2] - year1 + 1
    y4 <- nYear
    a1 <- 1
    a2 <- input$Fages
    a3 <- nAge
    Fg <- matrix(NA, nrow = nYear, ncol = nAge)
    Fg[y1,a1] <- input$Fy1a1
    Fg[y1,a2] <- input$Fy1a2
    Fg[y1,a3] <- input$Fy1a3
    Fg[y2,a1] <- input$Fy2a1
    Fg[y2,a2] <- input$Fy2a2
    Fg[y2,a3] <- input$Fy2a3
    Fg[y3,a1] <- input$Fy3a1
    Fg[y3,a2] <- input$Fy3a2
    Fg[y3,a3] <- input$Fy3a3
    Fg[y4,a1] <- input$Fy4a1
    Fg[y4,a2] <- input$Fy4a2
    Fg[y4,a3] <- input$Fy4a3
    if (a2 > (a1+1)){
      for (a in (a1+1):(a2-1)){
        Fg[y1,a] <- Fg[y1,a1] + (a - a1) * (Fg[y1,a2] - Fg[y1,a1]) / (a2 - a1)
        Fg[y2,a] <- Fg[y2,a1] + (a - a1) * (Fg[y2,a2] - Fg[y2,a1]) / (a2 - a1)
        Fg[y3,a] <- Fg[y3,a1] + (a - a1) * (Fg[y3,a2] - Fg[y3,a1]) / (a2 - a1)
        Fg[y4,a] <- Fg[y4,a1] + (a - a1) * (Fg[y4,a2] - Fg[y4,a1]) / (a2 - a1)
      }
    }
    if (a3 > (a2+1)){
      for (a in (a2+1):(a3-1)){
        Fg[y1,a] <- Fg[y1,a2] + (a - a2) * (Fg[y1,a3] - Fg[y1,a2]) / (a3 - a2)
        Fg[y2,a] <- Fg[y2,a2] + (a - a2) * (Fg[y2,a3] - Fg[y2,a2]) / (a3 - a2)
        Fg[y3,a] <- Fg[y3,a2] + (a - a2) * (Fg[y3,a3] - Fg[y3,a2]) / (a3 - a2)
        Fg[y4,a] <- Fg[y4,a2] + (a - a2) * (Fg[y4,a3] - Fg[y4,a2]) / (a3 - a2)
      }
    }
    if (y2 > (y1+1)){
      for (y in (y1+1):(y2-1)){
        for (a in 1:nAge){
          Fg[y,a] <- Fg[y1,a] + (y - y1) * (Fg[y2,a] - Fg[y1,a]) / (y2 - y1)
        }
      }
    }
    if (y3 > (y2+1)){
      for (y in (y2+1):(y3-1)){
        for (a in 1:nAge){
          Fg[y,a] <- Fg[y2,a] + (y - y2) * (Fg[y3,a] - Fg[y2,a]) / (y3 - y2)
        }
      }
    }
    if (y4 > (y3+1)){
      for (y in (y3+1):(y4-1)){
        for (a in 1:nAge){
          Fg[y,a] <- Fg[y3,a] + (y - y3) * (Fg[y4,a] - Fg[y3,a]) / (y4 - y3)
        }
      }
    }
    FAA$Fgrid <- Fg
    df <- data.frame()
    for (y in 1:nYear){
      thisdf <- data.frame(year = as.integer(y + year1 - 1),
                           age = 1:nAge,
                           Fval = Fg[y,])
      df <- rbind(df, thisdf)
    }
    FAA$Fdf <- df
    FAA$values <- Fg
    FAA
  })
  
  Wlist <- reactive({
    W <- list()
    WAA <- input$Winfyear1 * (1 - exp(-input$Kyear1 * seq(1, input$nAge))) ^ 3
    W$base <- matrix(rep(WAA, each=input$nYear), nrow=input$nYear, ncol=input$nAge)
    W$values <- W$base
    W
  })
  
  indexlist <- reactive({
    index <- list()
    for (ind in 1:input$nindices){
      index[[ind]] <- list()
      if (ind == 1){
        index[[ind]]$A50 <- input$i1A50
        leftc <- 1 / (1 + exp(-input$i1slope * (ages() - input$i1A50)))
        rtc <- 1 / (1 + exp(-input$i1slope2 * (ages() - input$i1A502)))
        index[[ind]]$selx <- leftc * rtc / max(leftc * rtc)
        index[[ind]]$q <- input$i1q
      }
    }
    index
  })
  
  Rlist <- reactive({
    r <- list()
    r$SRcurve <- "BevHolt"
    r$BHR0 <- input$R0
    r$BHsteepness <- input$steepness
    Mvec <- Mlist()$values[1,]  # use first year for calculations
    Wvec <- Wlist()$values[1,]  # use first year for calculations
    N0pr <- calcEquilibriumPop(1, input$nAge, Mvec, 0)
    SSB0pr <- calcAggregateBiomass(N0pr, Wvec, Mvec, 0, 0)  # assume SSB at start of year for now
    r$BHSSB0 <- SSB0pr * input$R0
    denom <- 5 * input$steepness - 1
    r$BHalpha <- 4 * input$steepness * input$R0 / denom
    r$BHbeta <- r$BHSSB0 * (1 - input$steepness) / denom
    r
  })
  
  Ny1list <- reactive({
    Ny1 <- list()
    Ny1$type <- "Equilibrium"
    Ny1$R <- "BevHolt"
    Ny1
  })
  
  output$dimPlot <- renderPlot({
    ya <- expand.grid(Age = ages(), Year = years())
    plot(ya$Age, ya$Year, xlab="Age", ylab="Year")
     title(paste("Number of Indices =", input$nindices))
   })
   
  output$Mplot <- renderPlot({
    matplot(rownames(Mlist()$values), Mlist()$values, xlab="Year", ylab="M", ylim=c(0,max(Mlist()$values)))
  })
   
  output$Ftable <- renderTable({
    Flist()$Fgrid
  })
  
  output$Wplot <- renderPlot({
    matplot(Wlist()$values)
  })
  
  output$indexplot <- renderPlot({
    plot(ages(), indexlist()[[1]]$selx, ylim=c(0,1), xlab="Age", ylab="Selectivity")
  })
  
  output$Nyear1plot <- renderPlot({
    Mvec <- Mlist()$values[1,]
    Fvec <- Flist()$values[1,]
    Wvec <- Wlist()$values[1,]
    NpR <- calcEquilibriumPop(1, input$nAge, Mvec, Fvec, input$plusgroupflag)
    SSBpR <- calcAggregateBiomass(NpR, Wvec) # assume SSB at start of year for now
    SSB1 <- Rlist()$BHalpha * SSBpR - Rlist()$BHbeta
    R1 <- Rlist()$BHalpha * SSB1 / (Rlist()$BHbeta + SSB1)
    Nyear1 <- calcEquilibriumPop(R1, input$nAge, Mvec, Fvec, input$plusgroupflag)
    Nyear1noise <- addLognormalError(Nyear1, input$Rsigma, input$Rsigmabiasflag)
    if (input$plusgroupflag == TRUE){
      Nyear1noise[input$nAge] <- Nyear1[input$nAge] # do not apply noise to plus group
    }
    plot(ages(), Nyear1noise, xlab="Age", ylab="Population N in Year 1", ylim=c(0,max(c(Nyear1, Nyear1noise))))
     lines(ages(), Nyear1)
  })
  
  output$Recruitmentplot <- renderPlot({
    ssb <- seq(0, Rlist()$BHSSB0, length.out = 400)
    r <- Rlist()$BHalpha * ssb / (Rlist()$BHbeta + ssb)
    rn <- addLognormalError(r, input$Rsigma, input$Rsigmabiasflag)
    plot(ssb, rn, xlab="SSB", ylab="Recruits (millions)")
     lines(ssb, r)
     title(main = "Demonstration of recruitment variability")
  })
  
  output$downloadinput <- downloadHandler(
    filename = function() {paste0("ShinyInput", Sys.time(), ".DMP")},
    content = function(file) {save(input, file=file)}
  )
  
  output$downloadoutput <- downloadHandler(
    filename = function() {paste0("ShinyOutput", Sys.time(), ".DMP")},
    content = function(file) {save(output, file=file)}
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

