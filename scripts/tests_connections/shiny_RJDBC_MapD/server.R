library(shiny)
# Define server logic required to draw a histogram


function(input, output) {
  
  conMapD <- dbConnect(drv, "jdbc:mapd:localhost:9091:mapd", "mapd", "HyperInteractive")
  fp <- tbl(conMapD, "fp")
  #on.exit(dbDisconnect(conMapD))
  
  output$distPlot <- renderPlot({
    data_fp <- fp %>%
      filter(sim_name == input$sim_name) %>%
      group_by(annee) %>%
      tally() %>%
      collect()
    
    ggplot(data_fp, aes(factor(annee), n)) +
      geom_point()
  })
}