library(shiny)

shinyServer(function(session, input, output) {
   
  filtred <- reactiveValues(agregats = NA,
                            FP_all = NA,
                            FP_summary = NA,
                            parameters = NA,
                            paroisses = NA,
                            poles = NA,
                            results = NA,
                            seigneurs = NA
                            )
  
  parameters_data <- reactive({
    nonUniqueParams <- JIAP_parameters %>%
      gather(key = "Var", value = "Value") %>%
      group_by(Var, Value) %>%
      mutate(Freq = n()) %>%
      filter(Freq != nrow(JIAP_parameters)) %>%
      extract2("Var") %>%
      unique(.)
    
    JIAP_parameters %>% select_( .dots = nonUniqueParams)
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
      filtred$agregats <- JIAP_agregats %>% filter(seed %in% filtredSeeds())
      filtred$FP_all <- JIAP_FP_all %>% filter(seed %in% filtredSeeds()) 
      filtred$FP_summary <- JIAP_FP_summary %>% filter(seed %in% filtredSeeds())
      filtred$parameters <- JIAP_parameters %>% filter(seed %in% filtredSeeds())
      filtred$paroisses <- JIAP_paroisses %>% filter(seed %in% filtredSeeds())
      filtred$poles <- JIAP_poles %>% filter(seed %in% filtredSeeds())
      filtred$results <- JIAP_results %>% filter(seed %in% filtredSeeds())
      filtred$seigneurs <- JIAP_seigneurs %>% filter(seed %in% filtredSeeds())
    } else {
      filtred$agregats <- NA
      filtred$FP_all <- NA
      filtred$FP_summary <- NA
      filtred$parameters <- NA
      filtred$paroisses <- NA
      filtred$poles <- NA
      filtred$results <- NA
      filtred$seigneurs <- NA
    }
  })

  output$paramParCoords <- renderParcoords({
    parcoords(parameters_data() %>% select(-seed, -name) %>% set_colnames(LETTERS[1:ncol(.)]),
              rownames=FALSE,
              brushMode="1d",
              reorderable = TRUE,
              autoresize = TRUE)
  })
  
  output$paramLegend <- renderDataTable({
    thoseParameters <- parameters_data() %>% select(-seed, -name)
    thisDF <- data_frame(Key = LETTERS[1:ncol(thoseParameters)], Value = colnames(thoseParameters))
    thisDF
  },options = list(pageLength = 5, lengthChange =  FALSE))
  
  output$dataVolume <- renderText({
    blob <- ifelse(length(input$paramParCoords_brushed_row_names)>0,
                   length(input$paramParCoords_brushed_row_names),
                   "Toutes les")
    sprintf("%s simulations sélectionnées sur un total de %s",
            blob, 
            nrow(JIAP_parameters))
  })
  
  ecarts_types <- reactive({
    JIAP_results %>%
      filter(Annee == 1160) %>%
      select(NbAgregats,NbChateaux, NbGrosChateaux, NbEglisesParoissiales, DistPpvEglises, TxFpIsoles,RatioChargeFiscale) %>%
      sapply(sd, simplify = TRUE)
  })
  
  summary_table <- reactive({
    JIAP_results %>%
      filter(Annee == 1160) %>%
      select(NbAgregats,NbChateaux, NbGrosChateaux, NbEglisesParoissiales, DistPpvEglises, TxFpIsoles,RatioChargeFiscale) %>%
      summary() %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      select(Var2, Freq) %>%
      separate(Freq,  into = c("Indice", "Valeur"),  sep = ":") %>%
      mutate(Valeur = as.numeric(Valeur)) %>%
      spread(Indice, Valeur) %>%
      rename(Indice = Var2, Q1 = `1st Qu.`, Q3 = `3rd Qu.`, Max = `Max.   `, Min = `Min.   `, Moyenne = `Mean   `, `Médiane` = `Median `) %>%
      mutate(StDev = as.numeric(ecarts_types())) %>%
      arrange(order(names(ecarts_types()))) %>%
      mutate(Objectif = c(200, 50, 10, 300, 3000, 0.2, 3)) %>%
      mutate(Ecart = (Moyenne / Objectif) -  1 ) %>%
      select(Indice, Objectif,Moyenne, Ecart, `Médiane`, Q1, Q3, StDev, Min, Max) %>%
      mutate(Indice = c("Nombre d'agrégats", "Nombre de châteaux",  "Nombre de gros châteaux",
                        "Nombre d'églises paroissiales", "Distance moyenne entre églises",  
                        "Proportion de FP isolés", "Augmentation de la charge fiscale (ratio)"))
  })
  
  ecarts_types2 <- reactive({
    filtred$results %>%
      filter(Annee == 1160) %>%
      select(NbAgregats,NbChateaux, NbGrosChateaux, NbEglisesParoissiales, DistPpvEglises, TxFpIsoles,RatioChargeFiscale) %>%
      sapply(sd, simplify = TRUE)
  })
  
  summary_table2 <- reactive({
    filtred$results %>%
      filter(Annee == 1160) %>%
      select(NbAgregats,NbChateaux, NbGrosChateaux, NbEglisesParoissiales, DistPpvEglises, TxFpIsoles,RatioChargeFiscale) %>%
      summary() %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      select(Var2, Freq) %>%
      separate(Freq,  into = c("Indice", "Valeur"),  sep = ":") %>%
      mutate(Valeur = as.numeric(Valeur)) %>%
      spread(Indice, Valeur) %>%
      rename(Indice = Var2, Q1 = `1st Qu.`, Q3 = `3rd Qu.`, Max = `Max.   `, Min = `Min.   `, Moyenne = `Mean   `, `Médiane` = `Median `) %>%
      mutate(StDev = as.numeric(ecarts_types2())) %>%
      arrange(order(names(ecarts_types2()))) %>%
      mutate(Objectif = c(200, 50, 10, 300, 3000, 0.2, 3)) %>%
      mutate(Ecart = (Moyenne / Objectif) -  1 ) %>%
      select(Indice, Objectif,Moyenne, Ecart, `Médiane`, Q1, Q3, StDev, Min, Max) %>%
      mutate(Indice = c("Nombre d'agrégats", "Nombre de châteaux",  "Nombre de gros châteaux",
                        "Nombre d'églises paroissiales", "Distance moyenne entre églises",  
                        "Proportion de FP isolés", "Augmentation de la charge fiscale (ratio)"))
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
  
  output$selectionTable <- renderFormattable({
    arrondir <- formatter("span",
                          style = function(x) style(round(x, digits = 2)))

    formattable(summary_table2(), list(
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
  
  source("plots/FP.R", local = TRUE, encoding = 'utf8')
  source("plots/Agregats.R", local = TRUE, encoding = 'utf8')
  source("plots/Seigneurs.R", local = TRUE, encoding = 'utf8')
  source("plots/Poles.R", local = TRUE, encoding = 'utf8')
  source("plots/Paroisses.R", local = TRUE, encoding = 'utf8')
  
  
  
  
  
  
})
