sensitivity <- reactiveValues(selected = NULL,
                              plotted = NULL)



# 
# plot_data <- filtered_data %>%
#   gather(key = Indicateur, value = Resultat, -param, -valeur, -seed) %>%
#   filter(param == sample_n(., 1) %>% pull(param)) %>%
#   mutate(Indicateur = stringr::str_replace(Indicateur, "_om", "")) %>%
#   mutate(Indicateur = stringr::str_replace_all(Indicateur, "_", " ")) %>%
#   mutate(Indicateur = stringr::str_wrap(Indicateur, width = 10))
# 
# plot_title <- plot_data %>% pull(param) %>% unique()
# 
# ggplot(plot_data, aes(valeur, Resultat, group = factor(valeur))) +
#   geom_tufteboxplot() +
#   facet_wrap( ~Indicateur, scales = "free", nrow = 1) +
#   labs(title = plot_title, x = "Valeurs de paramètres", y = "Valeurs des Indicateurs") +
#   theme(strip.text = element_text(size = 7),
#         axis.text = element_text(size = 7),
#         strip.background = element_blank())
# 
# ggplot(plot_data %>% group_by(Indicateur, valeur) %>% summarise(mean = median(Resultat, na.rm = TRUE))) +
#   geom_point(aes(valeur,  mean)) +
#   facet_wrap( ~Indicateur, scales = "free", nrow = 1)

output$sensitivity_summary <- DT::renderDataTable({
  sensibility_summary_table
}, extensions = "FixedColumns",style = "bootstrap", class = "table-condensed", rownames = TRUE,
 options = list(paging = FALSE, scrollX = TRUE, fixedColumns = TRUE))


observe({
  if (length(input$sensitivity_summary_rows_selected) > 0) {
    sensitivity$selected <- sensibility_summary_table[input$sensitivity_summary_rows_selected, "param"] %>% pull(param)
  } else {
    sensitivity$selected <- NULL
  }
})

observe({
  req(sensitivity$selected)
  for (currentParam in sensitivity$selected) {
    if (currentParam %in% sensitivity$plotted) {
       #do nothing
    } else {
      sensitivity$plotted <- c(sensitivity$plotted, currentParam)
      plotname <- paste("paramplot_",  currentParam, sep = "")
      insertUI(selector = "#sensitivity_plots",
               where = "beforeEnd",
               ui =  fluidRow(plotOutput(outputId = plotname,  height = "200px"))
      )
      output[[plotname]] <- renderPlot({
        plot_data <- filtered_data %>%
          filter(param == currentParam) %>%
          mutate(Indicateur = stringr::str_wrap(Indicateur, width = 20))
        
        ggplot(plot_data, aes(valeur, Resultat, group = factor(valeur))) +
          geom_tufteboxplot() +
          facet_wrap( ~Indicateur, scales = "free", nrow = 1) +
          labs(title = currentParam, x = "Valeurs de paramètres", y = "Valeurs des Indicateurs") +
          theme(strip.text = element_text(size = 7),
                axis.text = element_text(size = 7),
                strip.background = element_blank())
      })
    }
  }
})

observe({
  req(sensitivity$plotted)
  for (currentParam in sensitivity$plotted) {
    if (currentParam %in% sensitivity$selected) {
      # Do nothing
    } else {
      plotname <- paste("paramplot_",  currentParam, sep = "")
      removeUI(selector = sprintf('#%s', plotname))
      sensitivity$plotted <- sensitivity$plotted[sensitivity$plotted != currentParam]
    }
  }
})
