library(shiny)
fluidPage(
   
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "sim_name", label = "Name",
                    choices = c("4_4_A", "4_4_B", "4_5_A"), 
                    multiple = FALSE,
                    selected = "4_4_A")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)
