library(shiny)

shinyServer(function(session, input, output) {
  
  filtred <- reactiveValues(agregats = NA,
                            FP = NA,
                            parameters = NA,
                            paroisses = NA,
                            poles = NA,
                            results = NA,
                            seigneurs = NA
  )
  
  parameters_data <- reactive({
    nonUniqueParams <- sim_parameters %>%
      gather(key = "Var", value = "Value") %>%
      group_by(Var, Value) %>%
      mutate(Freq = n()) %>%
      filter(Freq != nrow(sim_parameters)) %>%
      pull("Var") %>%
      unique(.)
    
    sim_parameters %>% select(!!nonUniqueParams)
  })
  
  filtredSeeds <- reactive({
    if (length(input$paramParCoords_brushed_row_names) > 0){
      goodSeeds$seed[as.numeric(input$paramParCoords_brushed_row_names)]
    } else {
      goodSeeds$seed
    }
  })
  
  observe({
    if (length(input$paramParCoords_brushed_row_names) > 0){
      filtred$agregats <- sim_agregats %>% filter(seed %in% filtredSeeds())
      filtred$FP <- sim_FP %>% filter(seed %in% filtredSeeds()) 
      filtred$parameters <- sim_parameters %>% filter(seed %in% filtredSeeds())
      filtred$paroisses <- sim_paroisses %>% filter(seed %in% filtredSeeds())
      filtred$poles <- sim_poles %>% filter(seed %in% filtredSeeds())
      filtred$results <- sim_results %>% filter(seed %in% filtredSeeds())
      filtred$seigneurs <- sim_seigneurs %>% filter(seed %in% filtredSeeds())
    } else {
      filtred$agregats <- NA
      filtred$FP <- NA
      filtred$parameters <- NA
      filtred$paroisses <- NA
      filtred$poles <- NA
      filtred$results <- NA
      filtred$seigneurs <- NA
    }
  })
  
  # output$paramParCoords <- renderParcoords({
  #   parcoords(parameters_data() %>% select(-seed, -sim_name) %>% set_colnames(LETTERS[1:ncol(.)]),
  #             rownames=FALSE,
  #             brushMode="1d",
  #             reorderable = TRUE,
  #             autoresize = TRUE)
  # })
  # 
  # output$paramLegend <- renderDataTable({
  #   thoseParameters <- parameters_data() %>% select(-seed, -sim_name)
  #   thisDF <- data_frame(Key = LETTERS[1:ncol(thoseParameters)], Value = colnames(thoseParameters))
  #   thisDF
  # },options = list(pageLength = 5, lengthChange =  FALSE))
  
  output$dataVolume <- renderText({
    blob <- ifelse(length(input$paramParCoords_brushed_row_names)>0,
                   length(input$paramParCoords_brushed_row_names),
                   "Toutes les")
    sprintf("%s simulations sélectionnées sur un total de %s",
            blob, 
            nrow(sim_parameters))
  })
  
  
  summary_table <- reactive({
    
    Objectifs <- data_frame(
      Var = c("NbAgregats", "nbChateaux", "nbGdChateaux", "NbSeigneurs","nbEglisesParoissiales", "distance_eglises_paroissiales", "prop_FP_isoles", "RatioChargeFiscale"),
      RealVar = c("Agrégats", "Châteaux",  "Gros châteaux", "Seigneurs",
                  "Églises paroissiales", "Distance moyenne entre églises",  
                  "Part de foyers paysans isolés",
                  "Augmentation de la charge fiscale des foyers paysans"),
      Objectif = c(200, 50, 10, 200, 300, 3000, 0.2, 3),
      Ordre = 1:8
    )
    
    nbSeigneurs <- sim_seigneurs %>%
      group_by(seed, Annee) %>%
      summarise(NbSeigneurs = n())
      
    tableau_resultats <- sim_results %>%
      filter(Annee == 1160) %>%
      left_join(nbSeigneurs) %>%
      select(-seed) %>%
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
    req(filtred$results)
    Objectifs <- data_frame(
      Var = c("NbAgregats", "nbChateaux", "nbGdChateaux", "NbSeigneurs","nbEglisesParoissiales", "distance_eglises_paroissiales", "prop_FP_isoles", "RatioChargeFiscale"),
      RealVar = c("Agrégats", "Châteaux",  "Gros châteaux", "Seigneurs",
                  "Églises paroissiales", "Distance moyenne entre églises",  
                  "Part de foyers paysans isolés",
                  "Augmentation de la charge fiscale des foyers paysans"),
      Objectif = c(200, 50, 10, 200, 300, 3000, 0.2, 3),
      Ordre = 1:8
    )

    nbSeigneurs <- filtred$seigneurs %>%
      group_by(seed, Annee) %>%
      summarise(NbSeigneurs = n())

    tableau_resultats <- filtred$results %>%
      filter(Annee == 1160) %>%
      left_join(nbSeigneurs) %>%
      select(-seed) %>%
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
  
  # output$selectionTable <- renderPrint({
  #   filtredSeeds()
  # })
  
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
  
  source("src_plots/FP.R", local = TRUE, encoding = 'utf8')
  source("src_plots/Agregats.R", local = TRUE, encoding = 'utf8')
  source("src_plots/Seigneurs.R", local = TRUE, encoding = 'utf8')
  source("src_plots/Poles.R", local = TRUE, encoding = 'utf8')
  source("src_plots/Paroisses.R", local = TRUE, encoding = 'utf8')
  source("src_plots/download_plots.R", local = TRUE, encoding = 'utf8')
  
  
  
  
  
  
})
