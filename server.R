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
      Var = c("NbAgregats", "nbChateaux", "nbGdChateaux", "nbEglisesParoissiales", "distance_eglises_paroissiales", "prop_FP_isoles", "RatioChargeFiscale"),
      RealVar = c("Nombre d'agrégats", "Nombre de châteaux",  "Nombre de gros châteaux",
                  "Nombre d'églises paroissiales", "Distance moyenne entre églises",  
                  "Proportion de FP isolés", "Augmentation de la charge fiscale (ratio)"),
      Objectif = c(200, 50, 10, 300, 3000, 0.2, 3),
      Ordre = 1:7
    )
    
    sim_results %>%
      filter(Annee == 1160) %>%
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
      select(RealVar, Objectif,Moyenne, Mediane, Q1, Q3, StDev, Min, Max)
  })
  
  
  summary_table2 <- reactive({
    Objectifs <- data_frame(
      Var = c("NbAgregats", "nbChateaux", "nbGdChateaux", "nbEglisesParoissiales", "distance_eglises_paroissiales", "prop_FP_isoles", "RatioChargeFiscale"),
      RealVar = c("Nombre d'agrégats", "Nombre de châteaux",  "Nombre de gros châteaux",
                  "Nombre d'églises paroissiales", "Distance moyenne entre églises",  
                  "Proportion de FP isolés", "Augmentation de la charge fiscale (ratio)"),
      Objectif = c(200, 50, 10, 300, 3000, 0.2, 3),
      Ordre = 1:7
    )
    
    filtred$results %>%
      filter(Annee == 1160) %>%
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
      select(RealVar, Objectif,Moyenne, Mediane, Q1, Q3, StDev, Min, Max)
  })
  
  output$targetsTable <- renderFormattable({
    arrondir <- formatter("span", 
                          style = function(x) style(round(x, digits = 2)))
    
    formattable(summary_table(), list(
      Objectif = formatter("span", style = function(x){as.integer(x)}),
      Moyenne = arrondir,
      Ecart = formatter("span", style = function(x){
        ifelse(abs(x) > 0.3,  style(color = "red", font.weight = "bold"),
               ifelse(abs(x) < 0.1, style(color = "green", font.weight = "bold"), NA))},
        function(x) percent(x)),
      `Médiane` = arrondir,
      Q1 = arrondir,
      Q3 = arrondir,
      StDev = formatter("span", style = function(x) style(round(x, digits = 2))),
      Min = formatter("span", style = function(x){as.integer(x)}),
      Max = formatter("span", style = function(x){as.integer(x)})
    ))
  })
  
  # output$selectionTable <- renderPrint({
  #   filtredSeeds()
  # })
  
  # output$selectionTable <- renderFormattable({
  #   arrondir <- formatter("span",
  #                         style = function(x) style(round(x, digits = 2)))
  # 
  #   formattable(summary_table2(), list(
  #     Objectif = formatter("span", style = function(x){as.integer(x)}),
  #     Moyenne = arrondir,
  #     Ecart = formatter("span", style = function(x){
  #       ifelse(abs(x) > 0.3,  style(color = "red", font.weight = "bold"),
  #              ifelse(abs(x) < 0.1, style(color = "green", font.weight = "bold"), NA))},
  #       function(x) percent(x)),
  #     `Médiane` = arrondir,
  #     Q1 = arrondir,
  #     Q3 = arrondir,
  #     StDev = formatter("span", style = function(x) style(round(x, digits = 2))),
  #     Min = formatter("span", style = function(x){as.integer(x)}),
  #     Max = formatter("span", style = function(x){as.integer(x)})
  # 
  #   ))
  # })
  
  source("plots/FP.R", local = TRUE, encoding = 'utf8')
  source("plots/Agregats.R", local = TRUE, encoding = 'utf8')
  # source("plots/Seigneurs.R", local = TRUE, encoding = 'utf8')
  source("plots/Poles.R", local = TRUE, encoding = 'utf8')
  # source("plots/Paroisses.R", local = TRUE, encoding = 'utf8')
  
  
  
  
  
  
})
