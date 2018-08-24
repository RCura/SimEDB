plotDownloadRateUI <- function(id,  position = 'haut') {
  ns <- NS(id)
  spinnerColor <- if_else(position == "haut", "#C6E3EF", "#D69DA7")
  tagList(
    fluidRow(
      column(11,plotOutput(ns('plot'), width = "100%") %>% withSpinner(type = 7, color = spinnerColor)),
      column(1, align = "center",style='padding-left:0px;',
             fluidRow(
               downloadLink(outputId = ns("download_pdf"),
                            label = list(icon(name = "file-pdf-o fa-2x")),
                            title="Télécharger le graphique en PDF"),
               downloadLink(outputId = ns("download_png"),
                            label = list(icon(name = "file-image-o fa-2x")),
                            title="Télécharger le graphique en PNG")
             ),
             fluidRow(
               ratingInput(inputId = ns("rating"), label = "",
                           dataFilled = "fa fa-star fa-lg",
                           dataEmpty = "fa fa-star-o fa-lg"
                           )
             )
      )
    )
  )
}



plotDownloadRate <- function(input, output, session, plotFunction, plotName, user, seeds) {
  output$plot <- renderPlot({
    plotFunction()
  }, res = 150)
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0(plotName, ".pdf")
    },
    content = function(file) {
      ggsave(file, plotFunction(), width = 20, height = 10, units = "cm")
    }
  )
  
  output$download_png <- downloadHandler(
    filename = function() {
      paste0(plotName, ".png")
    },
    content = function(file) {
      ggsave(file, plotFunction(), width = 20, height = 10, units = "cm", dpi = 150)
    }
  )
  
  observeEvent(input$rating,{
    req(input$rating)
    currentTime <- Sys.time()
    formattedTime <- glue::glue("{y}-{m}-{d} {h}:{m}",
                                y = year(currentTime),
                                m = month(currentTime),
                                d = day(currentTime),
                                h = hour(currentTime),
                                m = minute(currentTime)
    )
    filtredSeeds <- as.character(seeds)
    nbSeeds <- length(filtredSeeds)
    rating <- tibble(User = character(nbSeeds),
                     Time = character(nbSeeds),
                     Input = character(nbSeeds),
                     Rating = numeric(nbSeeds),
                     Seed = character(nbSeeds)
                     ) %>%
      mutate(User = user,
             Time = formattedTime,
             Input = plotName,
             Rating = input$rating,
             Seed = filtredSeeds)

    write_csv(rating, path = "rating.csv", append = TRUE)
  })
}

# # Debug Module
# plotNbFP <- function(data) {
#   plotData <- data %>%
#     group_by(seed, sim_name, annee) %>%
#     summarise(Nb = n()) %>%
#     collect()
#   
#   ggplot(plotData) +
#     aes(x = factor(annee), y = Nb) +
#     geom_tufteboxplot()
# }