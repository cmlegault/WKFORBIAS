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
                     choices = list("User Input", "Single Value", "Constant by Age", "More"),
                     selected = "Single Value")
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
   
   output$dimPlot <- renderPlot({
     yr1 <- as.numeric(input$year1)
     plot(1:10,1:10,xlim=c(1, input$nages), ylim=c(yr1, yr1 + input$nyears - 1), type='n', xlab="Age", ylab="Year")
     title(paste("Number of Indices =", input$nindices))
   })
   
   output$Mplot <- renderPlot({
     maa <- matrix(0.2, nrow=input$nyears, ncol=input$nages)
     matplot(maa)
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

