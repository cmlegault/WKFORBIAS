#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram
ui <- navbarPage(strong("WKFORBIAS Set Up"),
  
   tabPanel("Dimensions",
     sidebarLayout(
       sidebarPanel(
         sliderInput("years",
                     "Years",
                     min = 1960,
                     max = 2050,
                     value = c(1991, 2018)),
         
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
         plotOutput("distPlot")
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
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$nages + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
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

