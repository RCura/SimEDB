# ---------------- Barplot of SimNames -----------------

count_experiments <- function(parameters_data){
  parameters_data %>%
    group_by(sim_name) %>%
    summarise(n = n_distinct(seed)) %>%
    collect()
}

output$simNames <- renderPlot({
  simNames <- count_experiments(sim$parameters)
  
  simNamesPlot <- ggplot(simNames, aes(x = sim_name, y = n)) +
    geom_col(fill = NA, alpha = 1, colour = "black", linetype = "dotted") +
    labs(x = "Expériences", y = "Nombre de simulations",
         title = "Distribution des simulations") +
    theme_minimal()
  
  if ( length( filtredHaut$parameters ) > 1 ) {
    simNamesPlot <- simNamesPlot +
      geom_col(data = count_experiments(filtredHaut$parameters),
               fill = "#43a2ca", alpha = 0.3, colour = "#053144")
  }
  
  if ( length( filtredBas$parameters ) > 1 ) {
    simNamesPlot <- simNamesPlot +
      geom_col(data = count_experiments(filtredBas$parameters),
               fill = "red", alpha = 0.3, colour = "#67000d")
  }
  simNamesPlot
})

# ---------------- ViolinPlots of Sim Results -----------------

Objectifs <- tibble(
  Var = c("nb_agregats", "nb_chateaux", "nb_grands_chateaux", "nb_seigneurs",
          "nb_eglises_paroissiales", "distance_eglises_paroissiales",
          "prop_fp_isoles", "ratio_charge_fiscale", "nb_fp", "densite_fp"),
  RealVar = c("Agrégats", "Châteaux",  "Gros châteaux", "Seigneurs",
              "Églises paroissiales", "Distance moyenne entre églises",  
              "Part de foyers paysans isolés","Augmentation de la charge fiscale des foyers paysans",
              "Nombre de Foyers Paysans", "Densité de population"),
  Objectif = c(200, 50, 10, 200, 300, 3000, 0.2, 3, NA, NA),
  Ordre = 1:10
) %>%
  mutate(Var = tolower(Var))

df_to_resultsPlot <- function(df){
  df %>%
    filter(annee == 1200) %>%
    select(-sim_name, -annee) %>%
    collect() %>%
    gather(key = Variable,
           value = Valeur) %>%
    left_join(Objectifs, by = c("Variable" = "Var")) %>%
    filter(!is.na(RealVar)) %>%
    mutate(VarCut = str_wrap(RealVar, width = 20)) %>%
    arrange(Ordre) %>%
    mutate(VarCut = factor(VarCut, levels = unique(VarCut))) %>%
    mutate(Valeur = as.numeric(Valeur))
}

output$resultsPlot <- renderPlot({

  resultsData <- df_to_resultsPlot(sim$results)
  
  resultPlot <- ggplot(resultsData %>% filter(Variable != "seed"), aes(VarCut, Valeur)) +
    geom_violin(scale = "area",  na.rm = TRUE, fill = NA , colour = "black", alpha = 0.3, linetype = "dotted") +
    facet_wrap(~ VarCut, scales = "free", nrow = 1) +
    theme_minimal() +
    theme(strip.background = element_blank(),
          strip.text = element_blank()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Indicateurs", y = "Valeur", title = "Distribution des indicateurs de sortie")
  
  if ( length( filtredHaut$results ) > 1 ) {
    resultsFiltredData <- df_to_resultsPlot(filtredHaut$results)
    
    resultPlot <- resultPlot +
      geom_violin(data = resultsFiltredData,
                  scale = "area",  na.rm = TRUE, fill = "#43a2ca", alpha = .3, colour = "#053144")
  }
  
  if ( length( filtredBas$results ) > 1 ) {
    resultsFiltredData <- df_to_resultsPlot(filtredBas$results)
    
    resultPlot <- resultPlot +
      geom_violin(data = resultsFiltredData,
                  scale = "area",  na.rm = TRUE, fill = "red", alpha = .3, colour = "#67000d")
  }
  
  resultPlot
  
})

# ---------------- Table of general targets -----------------

summarise_results <- function(reactiveList){
  nbSeigneurs <- reactiveList[["seigneurs"]] %>%
    filter(annee == 1200) %>%
    group_by(seed) %>%
    summarise(nb_seigneurs = n()) %>%
    collect()
  
  surface_monde <- reactiveList[["parameters"]] %>%
    select(seed, taille_cote_monde) %>%
    collect() %>%
    mutate(superficie_monde = as.numeric(taille_cote_monde)^2) %>%
    select(-taille_cote_monde)
  
  reactiveList[["results"]] %>%
    filter(annee == 1200) %>%
    select(-annee) %>%
    collect() %>%
    left_join(nbSeigneurs, by = c("seed")) %>%
    left_join(surface_monde, by = "seed") %>%
    mutate(densite_fp = nb_fp / superficie_monde) %>%
    select(-seed, -superficie_monde) %>%
    rename_all(funs(gsub(x = ., pattern = "_", replacement = "."))) %>%
    summarise_if(is.numeric,funs(
      Moyenne = mean,
      Mediane = median,
      Q1 = quantile(., na.rm = TRUE, probs = .25),
      Q3 = quantile(., na.rm = TRUE, probs = .75),
      StDev = sd,
      Min = min,
      Max = max
    ), na.rm = TRUE
    ) %>%
    gather(key = Var, value = Value) %>%
    separate(Var, sep = "_", into = c("Var", "Indice")) %>%
    mutate(Var = gsub(Var, pattern = ".", replacement = "_", fixed = TRUE)) %>%
    left_join(Objectifs, by = "Var") %>%
    filter(!is.na(RealVar)) %>%
    spread(key = Indice,value = Value) %>%
    arrange(Ordre) %>%
    select(RealVar, Objectif,Moyenne, Mediane, Q1, Q3, StDev) %>%
    rename(Indicateur = RealVar,
           `Valeur attendue` = Objectif,
           `Médiane` = Mediane,
           `1er quartile` = Q1,
           `3ème quartile` = Q3,
           `Écart-type` = StDev)
}

summary_table_Haut <- reactive({
  req(filtredHaut$results)
  summarise_results(filtredHaut)
})

summary_table_Bas <- reactive({
  req(filtredBas$results)
  summarise_results(filtredBas)
})

pretty_format_summary_table <- function(df){
  formattable(df, table.attr = 'class="table table-striped"',
              list(
                area(row = 1:5, col = 2:6) ~ round,
                area(row = 1:5, col = 7) ~ formatter("span", function(x){ round(x, digits = 2) }),
                area(row = 6, col = 2:7) ~ formatter("span",  function(x){paste(round(x), "m")}),
                area(row = 7, col = 2:6) ~ formatter("span",  function(x){paste(round(x * 100), "%")}),
                area(row = 7, col = 7) ~ formatter("span",  function(x){paste(round(x * 100, digits = 1), "%")}),
                area(row = 8, col = 2:6) ~ formatter("span", function(x){ paste("x", round(x,digits = 1)) }),
                area(row = 8, col = 7) ~ formatter("span", function(x){ paste("x", round(x, digits = 2)) }),
                area(row = 9:10, col = 2) ~ formatter("span", function(x){ ifelse(is.na(x), yes = "-", "??") }),
                area(row = 9, col = 3:7) ~ round,
                area(row = 10, col = 3:7) ~ formatter("span", function(x){ round(x, digits = 2) })
              ))
}

output$summaryTable_Haut <- renderFormattable({
  req(summary_table_Haut())
  pretty_format_summary_table(summary_table_Haut())
})

output$summaryTable_Bas <- renderFormattable({
  req(summary_table_Bas())
  pretty_format_summary_table(summary_table_Bas())
})

# ---------------- Parallel Coordinates Plot -----------------


output$dataVolumeHaut <- renderText({
  blob <- filtredHaut$parameters %>% count() %>% collect() %>% pull()
  sprintf("%s simulations sélectionnées sur un total de %s",
          blob,
          sim$parameters %>% count() %>% collect() %>% pull())
})

output$dataVolumeBas <- renderText({
  blob <- filtredBas$parameters %>% count() %>% collect() %>% pull()
  sprintf("%s simulations sélectionnées sur un total de %s",
          blob,
          sim$parameters %>% count() %>% collect() %>% pull())
})



output$paramPC_Haut <- renderPlotly({
  parcoords_data <- parameters_data() %>%
    arrange(seed) %>%
    rename_all(.funs = funs(str_replace_all(., pattern = "_", replacement = "_")))
  
  parcoords_dims <- map((1:ncol(parcoords_data)), ~create_dims(parcoords_data, .x))

  p <-  plot_ly(source = 'parcoords_haut') %>%
    add_trace(data = parcoords_data,
              type = 'parcoords',
              dimensions = parcoords_dims,
              line = list(color = "blue")
    )

  onRender(p, "function(el, x) {
    el.on('plotly_restyle', function(d) {
      var blob = el.data[0].dimensions.map(function(x){return({label: x.label, constraintrange: x.constraintrange})});
      Shiny.setInputValue('plotly_brushed_haut', JSON.stringify(blob));
    });
  }")
})

output$paramPC_Bas <- renderPlotly({
  parcoords_data <- parameters_data() %>%
    arrange(seed) %>%
    rename_all(.funs = funs(str_replace_all(., pattern = "_", replacement = "_")))
  
  parcoords_dims <- map((1:ncol(parcoords_data)), ~create_dims(parcoords_data, .x))
  
  p <-  plot_ly(source = 'parcoords_bas') %>%
    add_trace(data = parcoords_data,
              type = 'parcoords',
              dimensions = parcoords_dims,
              line = list(color = "blue")
    )
  
  onRender(p, "function(el, x) {
           el.on('plotly_restyle', function(d) {
           var blob = el.data[0].dimensions.map(function(x){return({label: x.label, constraintrange: x.constraintrange})});
           Shiny.setInputValue('plotly_brushed_bas', JSON.stringify(blob));
           });
}")
})

# 
# output$paramParCoordsBas <- renderParcoords({
#   parcoords(parameters_data() %>%
#               arrange(seed) %>%
#               select(-seed) %>%
#               rename_all(.funs = funs(str_replace_all(., pattern = "_", replacement = " "))),
#             color = list(
#               colorBy = "sim name",
#               colorScale = htmlwidgets::JS('d3.scale.category10()')
#             ),
#             rownames = FALSE,
#             brushMode = "1d",
#             reorderable = TRUE,
#             autoresize = TRUE,
#             margin = list(top = 50, bottom = 10, left = 50, right = 10))
# })
