library(shiny)

shinyServer(function(session, input, output) {
  
  oldBrushedHaut <- reactiveVal(value = NA)
  oldBrushedBas <- reactiveVal(value = NA)
  
  sim <- reactiveValues(agregats = agregats,
                        FP =  fp,
                        parameters = parameters,
                        paroisses = paroisses,
                        poles = poles,
                        results = results,
                        seigneurs = seigneurs,
                        seeds = seeds)
  
  filtredHaut <- reactiveValues(agregats = NULL,
                                FP = NULL,
                                parameters = NULL,
                                paroisses = NULL,
                                poles = NULL,
                                results = NULL,
                                seigneurs = NULL
  )
  
  filtredBas <- reactiveValues(agregats = NULL,
                               FP = NULL,
                               parameters = NULL,
                               paroisses = NULL,
                               poles = NULL,
                               results = NULL,
                               seigneurs = NULL
  )
  
  observe({
    req(input$selectedSims)
    
    simNames <- input$selectedSims
    
    sim$seeds <- seeds %>% filter(sim_name %in% simNames)
    sim$agregats <- agregats %>% filter(sim_name %in% simNames)
    sim$FP <- fp %>% filter(sim_name %in% simNames)
    sim$parameters <- parameters %>% filter(sim_name %in% simNames)
    sim$paroisses <- paroisses %>% filter(sim_name %in% simNames)
    sim$poles <- poles %>% filter(sim_name %in% simNames)
    sim$results <- results %>% filter(sim_name %in% simNames)
    sim$seigneurs <- seigneurs %>% filter(sim_name %in% simNames)
  })
  
  parameters_data <- reactive({
    req(sim$parameters)
    tmp_parameters <- sim$parameters %>% collect()
    nonUniqueParams <- tmp_parameters %>%
      gather(key = "Var", value = "Value") %>%
      group_by(Var, Value) %>%
      mutate(Freq = n()) %>%
      ungroup() %>%
      filter(Freq != nrow(tmp_parameters)) %>%
      distinct(Var) %>%
      pull(Var)
    
    tmp_parameters %>% dplyr::select(!!nonUniqueParams)
  })
  
  filtredSeedsHaut <- reactive({
    req(input$paramParCoords_brushed_row_names, sim$seeds)
    tmp_seeds <- sim$seeds %>% collect()
    nbBrushed <- length(input$paramParCoords_brushed_row_names)
    nbTotal <- tmp_seeds %>% nrow()
    if (nbBrushed > 0 && nbBrushed < nbTotal && oldBrushedHaut() != nbBrushed) {
      tmp_seeds$seed[as.numeric(input$paramParCoords_brushed_row_names)]
    } else {
      oldBrushedHaut(nbBrushed)
      NULL
    }
  })
  
  filtredSeedsBas <- reactive({
    req(input$paramParCoordsBas_brushed_row_names, sim$seeds)
    tmp_seeds <- sim$seeds %>% collect()
    nbBrushed <- length(input$paramParCoordsBas_brushed_row_names)
    nbTotal <- tmp_seeds %>% nrow()
    if (nbBrushed > 0 && nbBrushed < nbTotal && oldBrushedBas() != nbBrushed) {
      tmp_seeds$seed[as.numeric(input$paramParCoordsBas_brushed_row_names)]
    } else {
      oldBrushedBas(nbBrushed)
      NULL
    }
  })
  
  observe({
    if (length(filtredSeedsHaut()) > 0) {
      brushedSeeds <- tibble(seed = filtredSeedsHaut())
      filtredHaut$agregats <- sim$agregats %>% inner_join(brushedSeeds, by = "seed", copy = TRUE)
      filtredHaut$FP <- sim$FP %>% inner_join(brushedSeeds, by = "seed", copy = TRUE)
      filtredHaut$parameters <- sim$parameters %>% inner_join(brushedSeeds, by = "seed", copy = TRUE)
      filtredHaut$paroisses <- sim$paroisses %>% inner_join(brushedSeeds, by = "seed", copy = TRUE)
      filtredHaut$poles <- sim$poles %>% inner_join(brushedSeeds, by = "seed", copy = TRUE)
      filtredHaut$results <- sim$results %>% inner_join(brushedSeeds, by = "seed", copy = TRUE)
      filtredHaut$seigneurs <- sim$seigneurs %>% inner_join(brushedSeeds, by = "seed", copy = TRUE)
    } else {
      filtredHaut$agregats <- sim$agregats
      filtredHaut$FP <- sim$FP
      filtredHaut$parameters <- sim$parameters
      filtredHaut$paroisses <- sim$paroisses
      filtredHaut$poles <- sim$poles
      filtredHaut$results <- sim$results
      filtredHaut$seigneurs <- sim$seigneurs
    }
  })
  
  observe({
    if (length(filtredSeedsBas()) > 0) {
      brushedSeeds <- tibble(seed = filtredSeedsBas())
      filtredBas$agregats <- sim$agregats %>% inner_join(brushedSeeds, by = "seed", copy = TRUE)
      filtredBas$FP <- sim$FP %>% inner_join(brushedSeeds, by = "seed", copy = TRUE)
      filtredBas$parameters <- sim$parameters %>% inner_join(brushedSeeds, by = "seed", copy = TRUE)
      filtredBas$paroisses <- sim$paroisses %>% inner_join(brushedSeeds, by = "seed", copy = TRUE)
      filtredBas$poles <- sim$poles %>% inner_join(brushedSeeds, by = "seed", copy = TRUE)
      filtredBas$results <- sim$results %>% inner_join(brushedSeeds, by = "seed", copy = TRUE)
      filtredBas$seigneurs <- sim$seigneurs %>% inner_join(brushedSeeds, by = "seed", copy = TRUE)
    } else {
      filtredBas$agregats <- NULL
      filtredBas$FP <- NULL
      filtredBas$parameters <- NULL
      filtredBas$paroisses <- NULL
      filtredBas$poles <- NULL
      filtredBas$results <- NULL
      filtredBas$seigneurs <- NULL
    }
  })
  
  output$paramParCoords <- renderParcoords({
    parcoords(parameters_data() %>% select(-seed),
              color = list(
                colorBy = "sim_name",
                colorScale = htmlwidgets::JS('d3.scale.category10()')
              ),
              rownames = FALSE,
              brushMode = "1d",
              reorderable = TRUE,
              autoresize = TRUE)
  })
  
  output$paramParCoordsBas <- renderParcoords({
    parcoords(parameters_data() %>% select(-seed),
              color = list(
                colorBy = "sim_name",
                colorScale = htmlwidgets::JS('d3.scale.category10()')
              ),
              rownames = FALSE,
              brushMode = "1d",
              reorderable = TRUE,
              autoresize = TRUE)
  })
  
  output$dataVolumeHaut <- renderText({
    blob <- length(input$paramParCoords_brushed_row_names)
    sprintf("%s simulations sélectionnées sur un total de %s",
            blob,
            sim$parameters %>% count() %>% collect() %>% pull())
  })
  
  output$dataVolumeBas <- renderText({
    blob <- length(input$paramParCoordsBas_brushed_row_names)
    sprintf("%s simulations sélectionnées sur un total de %s",
            blob,
            sim$parameters %>% count() %>% collect() %>% pull())
  })
  
  
  summary_table <- reactive({
    req(filtredHaut$results)
    
    Objectifs <- data_frame(
      Var = c("NbAgregats", "nbChateaux", "nbGdChateaux", "NbSeigneurs","nbEglisesParoissiales", "distance_eglises_paroissiales", "prop_FP_isoles", "RatioChargeFiscale"),
      RealVar = c("Agrégats", "Châteaux",  "Gros châteaux", "Seigneurs",
                  "Églises paroissiales", "Distance moyenne entre églises",  
                  "Part de foyers paysans isolés",
                  "Augmentation de la charge fiscale des foyers paysans"),
      Objectif = c(200, 50, 10, 200, 300, 3000, 0.2, 3),
      Ordre = 1:8
    ) %>%
      mutate(Var = tolower(Var))
    
    nbSeigneurs <- filtredHaut$seigneurs %>%
      group_by(seed, annee) %>%
      summarise(nbseigneurs = n())
    
    tableau_resultats <- filtredHaut$results %>%
      filter(annee == 1160) %>%
      left_join(nbSeigneurs, by = c("seed", "annee")) %>%
      select(-seed) %>%
      collect() %>%
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
    
    return(tableau_resultats)
  })
  
  
  summary_table2 <- reactive({
    req(filtredBas$results)
    Objectifs <- data_frame(
      Var = c("NbAgregats", "nbChateaux", "nbGdChateaux", "NbSeigneurs","nbEglisesParoissiales", "distance_eglises_paroissiales", "prop_FP_isoles", "RatioChargeFiscale"),
      RealVar = c("Agrégats", "Châteaux",  "Gros châteaux", "Seigneurs",
                  "Églises paroissiales", "Distance moyenne entre églises",  
                  "Part de foyers paysans isolés",
                  "Augmentation de la charge fiscale des foyers paysans"),
      Objectif = c(200, 50, 10, 200, 300, 3000, 0.2, 3),
      Ordre = 1:8
    ) %>%
      mutate(Var = tolower(Var))
    
    nbSeigneurs <- filtredBas$seigneurs %>%
      group_by(seed, annee) %>%
      summarise(nbseigneurs = n())
    
    tableau_resultats <- filtredBas$results %>%
      filter(annee == 1160) %>%
      left_join(nbSeigneurs, by = c("seed", "annee")) %>%
      select(-seed) %>%
      collect() %>%
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
    
  })
  
  output$targetsTable <- renderFormattable({
    req(summary_table())
    formattable(summary_table(), table.attr = 'class="table table-striped"',
                list(
                  area(row = 1:5, col = 2:6) ~ round,
                  area(row = 1:5, col = 7) ~ formatter("span", function(x){ round(x, digits = 2) }),
                  area(row = 6, col = 2:7) ~ formatter("span",  function(x){paste(round(x), "m")}),
                  area(row = 7, col = 2:6) ~ formatter("span",  function(x){paste(round(x * 100), "%")}),
                  area(row = 7, col = 7) ~ formatter("span",  function(x){paste(round(x * 100, digits = 1), "%")}),
                  area(row = 8, col = 2:6) ~ formatter("span", function(x){ paste("x", round(x,digits = 1)) }),
                  area(row = 8, col = 7) ~ formatter("span", function(x){ paste("x", round(x, digits = 2)) })
                ))
  })
  
  output$selectionTable <- renderFormattable({
    req(summary_table2())
    formattable(summary_table2(), table.attr = 'class="table table-striped"',
                list(
                  area(row = 1:5, col = 2:6) ~ round,
                  area(row = 1:5, col = 7) ~ formatter("span", function(x){ round(x, digits = 2) }),
                  area(row = 6, col = 2:7) ~ formatter("span",  function(x){paste(round(x), "m")}),
                  area(row = 7, col = 2:6) ~ formatter("span",  function(x){paste(round(x * 100), "%")}),
                  area(row = 7, col = 7) ~ formatter("span",  function(x){paste(round(x * 100, digits = 1), "%")}),
                  area(row = 8, col = 2:6) ~ formatter("span", function(x){ paste("x", round(x)) }),
                  area(row = 8, col = 7) ~ formatter("span", function(x){ paste("x", round(x, digits = 2)) })
                ))
  })
  
  output$simNames <- renderPlot({
    simNames <- sim$parameters %>%
      group_by(sim_name) %>%
      summarise(n = n()) %>%
      collect()
    
    simNamesPlot <- ggplot(simNames, aes(x = sim_name, y = n)) +
      geom_col(fill = NA, alpha = 1, colour = "black", linetype = "dotted") +
      labs(x = "Expériences", y = "Nombre de simulations",
           title = "Distribution des simulations") +
      theme_minimal()
    
    if ( length( filtredHaut$parameters ) > 1 ) {
      simNamesFiltred <- filtredHaut$parameters %>%
        group_by(sim_name) %>%
        summarise(n = n()) %>%
        collect()
      
      simNamesPlot <- simNamesPlot +
        geom_col(data = simNamesFiltred,
                 fill = "#43a2ca", alpha = 0.3, colour = "#053144")
    }
    
    if ( length( filtredBas$parameters ) > 1 ) {
      simNamesFiltred <- filtredBas$parameters %>%
        group_by(sim_name) %>%
        summarise(n = n()) %>%
        collect()
      
      simNamesPlot <- simNamesPlot +
        geom_col(data = simNamesFiltred,
                 fill = "red", alpha = 0.3, colour = "#67000d")
    }
    simNamesPlot
  })
  
  output$resultsPlot <- renderPlot({
    
    Objectifs <- data_frame(
      Var = c("NbAgregats", "nbChateaux", "nbGdChateaux", "NbSeigneurs","nbEglisesParoissiales", "distance_eglises_paroissiales", "prop_FP_isoles", "RatioChargeFiscale"),
      RealVar = c("Agrégats", "Châteaux",  "Gros châteaux", "Seigneurs",
                  "Églises paroissiales", "Distance moyenne entre églises",  
                  "Part de foyers paysans isolés",
                  "Augmentation de la charge fiscale des foyers paysans"),
      Objectif = c(200, 50, 10, 200, 300, 3000, 0.2, 3),
      Ordre = 1:8
    ) %>%
      mutate(Var = tolower(Var))
    
    resultsData <- sim$results %>%
      filter(annee == 1160) %>%
      select(-sim_name, -annee) %>%
      collect() %>%
      gather(key = Variable,
             value = Valeur) %>%
      left_join(Objectifs, by = c("Variable" = "Var")) %>%
      filter(!is.na(RealVar)) %>%
      mutate(VarCut = str_wrap(RealVar, width = 20)) %>%
      arrange(Ordre) %>%
      mutate(VarCut = factor(VarCut, levels = unique(VarCut)))
    
    resultPlot <- ggplot(resultsData %>% filter(Variable != "seed"), aes(VarCut, Valeur)) +
      geom_violin(scale = "area",  na.rm = TRUE, fill = NA , colour = "black", alpha = 0.3, linetype = "dotted") +
      facet_wrap(~ VarCut, scales = "free", nrow = 1) +
      theme_minimal() +
      theme(strip.background = element_blank(),
            strip.text = element_blank()) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Indicateurs", y = "Valeur", title = "Distribution des indicateurs de sortie")
    
    if ( length( filtredHaut$results ) > 1 ) {
      resultsFiltredData <- filtredHaut$results %>%
        filter(annee == 1160) %>%
        select(-sim_name, -annee) %>%
        collect() %>%
        gather(key = Variable,
               value = Valeur) %>%
        left_join(Objectifs, by = c("Variable" = "Var")) %>%
        filter(!is.na(RealVar)) %>%
        mutate(VarCut = str_wrap(RealVar, width = 20)) %>%
        arrange(Ordre) %>%
        mutate(VarCut = factor(VarCut, levels = unique(VarCut)))
      
      resultPlot <- resultPlot +
        geom_violin(data = resultsFiltredData,
                    scale = "area",  na.rm = TRUE, fill = "#43a2ca", alpha = .3, colour = "#053144")
    }
    
    if ( length( filtredBas$results ) > 1 ) {
      resultsFiltredData <- filtredBas$results %>%
        filter(annee == 1160) %>%
        select(-sim_name, -annee) %>%
        collect() %>%
        gather(key = Variable,
               value = Valeur) %>%
        left_join(Objectifs, by = c("Variable" = "Var")) %>%
        filter(!is.na(RealVar)) %>%
        mutate(VarCut = str_wrap(RealVar, width = 20)) %>%
        arrange(Ordre) %>%
        mutate(VarCut = factor(VarCut, levels = unique(VarCut)))
      
      resultPlot <- resultPlot +
        geom_violin(data = resultsFiltredData,
                    scale = "area",  na.rm = TRUE, fill = "red", alpha = .3, colour = "#67000d")
    }
    
    resultPlot
    
  })
  
  source("src_plots/FP.R", local = TRUE, encoding = 'utf8')
  source("src_plots/Agregats.R", local = TRUE, encoding = 'utf8')
  source("src_plots/Seigneurs.R", local = TRUE, encoding = 'utf8')
  source("src_plots/Poles.R", local = TRUE, encoding = 'utf8')
  source("src_plots/Paroisses.R", local = TRUE, encoding = 'utf8')
  source("src_plots/download_plots.R", local = TRUE, encoding = 'utf8')
  source("src_plots/rating.R", local = TRUE, encoding = "utf8")
  
  
  session$onSessionEnded(function() {
    dbDisconnect(conMonetDB, shutdown = TRUE)
    # MonetDBLite::monetdblite_shutdown()
  })
  
  
})
