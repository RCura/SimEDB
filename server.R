library(shiny)

shinyServer(function(session, input, output) {
  
  # Connection to DB system (for MapD at least)
  # drv declared in global
  conMapD <- dbConnect(drv, "jdbc:mapd:mapdi.cura.info:9091:mapd", "mapd", "HyperInteractive")
  # seeds <- tbl(conMapD, "seeds_5_1")
  # agregats <- tbl(conMapD, "agregats_5_1")
  # fp <- tbl(conMapD, "fp_5_1")
  # parameters <- tbl(conMapD, "parameters_5_1")
  # paroisses <- tbl(conMapD, "paroisses_5_1")
  # poles <- tbl(conMapD, "poles_5_1")
  # results <- tbl(conMapD, "results_5_1")
  # seigneurs <- tbl(conMapD, "seigneurs_5_1")
  
  
  # seeds <- tbl(conMapD, "seeds_6_1")
  # agregats <- tbl(conMapD, "agregats_6_1")
  # fp <- tbl(conMapD, "fp_6_1")
  # parameters <- tbl(conMapD, "parameters_6_1")
  # paroisses <- tbl(conMapD, "paroisses_6_1")
  # poles <- tbl(conMapD, "poles_6_1")
  # results <- tbl(conMapD, "global_6_1")
  # seigneurs <- tbl(conMapD, "seigneurs_6_1")
  # chateaux <- tbl(conMapD, "chateaux_6_1")
  
  seeds <- tbl(conMapD, "seeds_6_3")
  agregats <- tbl(conMapD, "agregats_6_3")
  fp <- tbl(conMapD, "fp_6_3")
  parameters <- tbl(conMapD, "parameters_6_3")
  paroisses <- tbl(conMapD, "paroisses_6_3")
  poles <- tbl(conMapD, "poles_6_3")
  results <- tbl(conMapD, "global_6_3")
  seigneurs <- tbl(conMapD, "seigneurs_6_3")
  chateaux <- tbl(conMapD, "chateaux_6_3")
  
  
  # ---------------- Declare reactive values -----------------
  
  oldBrushedHaut <- reactiveVal(value = NA)
  oldBrushedBas <- reactiveVal(value = NA)
  
  sim <- reactiveValues(agregats = agregats, FP =  fp, parameters = parameters,
                        paroisses = paroisses, poles = poles, results = results,
                        seigneurs = seigneurs, seeds = seeds)
  
  filtredHaut <- reactiveValues(agregats = NULL, FP = NULL, parameters = NULL,
                                paroisses = NULL, poles = NULL,
                                results = NULL, seigneurs = NULL)
  
  filtredBas <- reactiveValues(agregats = NULL, FP = NULL, parameters = NULL,
                               paroisses = NULL, poles = NULL,
                               results = NULL, seigneurs = NULL
  )
  
  tablesParams <- reactiveValues(haut = NULL, bas = NULL, hautTxt = NULL, basTxt = NULL)
  
  # ---------------- Globally filtered experiments -----------------
  
  ############### DEBUG #################
  # output$selected_seeds_Bas <- renderPrint({
  #   req(input$paramParCoordsBas_brushed_row_names, sim$seeds)
  #   tmp_seeds <- sim$seeds %>% collect() %>% arrange(seed)
  #   dput(tmp_seeds$seed[as.numeric(input$paramParCoordsBas_brushed_row_names)])
  # })
  # 
  # output$selected_seeds_Haut <- renderPrint({
  #   req(input$paramParCoordsHaut_brushed_row_names, sim$seeds)
  #   tmp_seeds <- sim$seeds %>% collect() %>% arrange(seed)
  #   input$paramParCoordsHaut_brushed_row_names
  # })
  
  selected_experiments <- reactive({
    if (is.null(input$selectedSims)){
      NULL
    } else {
      input$selectedSims
    }
  })
  
  selected_experiments_debounced <- selected_experiments %>% debounce(500)
  
  observe({
    req(selected_experiments_debounced())
    
    simNames <- selected_experiments_debounced()
    
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
    # nonUniqueParams <- tmp_parameters %>%
    #   gather(key = "Var", value = "Value") %>%
    #   group_by(Var, Value) %>%
    #   mutate(Freq = n()) %>%
    #   ungroup() %>%
    #   filter(Freq != nrow(tmp_parameters)) %>%
    #   distinct(Var) %>%
    #   pull(Var)
    
    sim_names <- tmp_parameters %>%
      select(seed, sim_name) %>%
      rename(valeur = sim_name) %>%
      mutate(parametre = "sim_name") %>%
      select(seed, parametre, valeur) %>%
      distinct(seed, parametre, valeur)
    
    nonUniqueParams <- tmp_parameters %>%
      select(-sim_name) %>%
      bind_rows(sim_names) %>%
      group_by(parametre) %>%
      summarise(nb = n_distinct(valeur)) %>%
      filter(nb > 1) %>%
      distinct(parametre) %>%
      pull(parametre)
    
    tmp_parameters %>%
      select(-sim_name) %>%
      bind_rows(sim_names) %>%
      filter(parametre %in% nonUniqueParams) %>%
      spread(parametre, valeur)
    
   # tmp_parameters %>% dplyr::select(!!nonUniqueParams)
  })
  
  # ---------------- Parallel Coordinates Filtering -----------------

  
  filtredSeedsHaut_plotly <- reactive({
    req(sim$seeds)
    
    if (is.null(input$plotly_brushed_haut)){
      return(list())
    }
    
    expressions_filtres <- expression_from_input(input$plotly_brushed_haut)
    colonnes_filtres <- parametres_from_input(input$plotly_brushed_haut)
    
    if ("sim_name" %in% colonnes_filtres){
      simNames <- NULL
    } else {
      simNames <- isolate(selected_experiments_debounced()) %>%
        as_tibble() %>%
        set_colnames("sim_name") %>%
        gather(parametre, valeurs) %>%
        distinct() %>%
        arrange(parametre, valeurs)
    }
    
    
    # toutNum <- function(x){
    #   suppressWarnings(all(!is.na(as.numeric(as.character(x)))))
    # }
    # '%ni%' <- Negate('%in%')
    # 
    # #numericParameters <- params_data %>%
    # numericParameters <- parameters_data() %>%
    #   select(-seed) %>%
    #   gather(Param, Valeur) %>%
    #   group_by(Param, Valeur) %>%
    #   tally() %>%
    #   group_by(Param) %>%
    #   summarise(num = if_else(toutNum(Valeur), TRUE, FALSE)) %>%
    #   filter(num) %>%
    #   pull(Param)
    # 
    # #characterParameters <- params_data %>%
    # characterParameters <- parameters_data() %>%
    #   select(-seed) %>%
    #   gather(Param, Valeur) %>%
    #   distinct(Param) %>%
    #   filter(Param %ni% numericParameters) %>%
    #   pull(Param)
    
    #selected_table <- params_data %>%
    selected_table <- parameters_data() %>%
      mutate(seed = as.numeric(seed)) %>%
      # mutate_at(vars(numericParameters), as.numeric) %>%
      # mutate_at(vars(characterParameters), char_to_num) %>%
      mutate_if(is.character, funs(char = char_to_num)) %>%
      filter(!!!expressions_filtres)
    
    if (length(colonnes_filtres) > 0){
      tablesParams$haut <- selected_table %>%
        select(!!!colonnes_filtres) %>%
        gather(key = parametre, value = valeurs) %>%
        mutate(valeurs = as.character(valeurs)) %>%
        arrange(parametre, valeurs) %>%
        bind_rows(simNames) %>%
        distinct(parametre, valeurs) %>%
        group_by(parametre) %>%
        nest() %>%
        mutate(valeurs = map_chr(data, ~ unlist(.x, use.names = FALSE) %>% paste(., collapse = " ; "))) %>%
        select(parametre, valeurs)
    } else {
      tablesParams$haut <- simNames %>%
        group_by(parametre) %>%
        nest() %>%
        mutate(valeurs = map_chr(data, ~ unlist(.x, use.names = FALSE) %>% paste(., collapse = " ; "))) %>%
        select(parametre, valeurs)
    }
    

    selected_seeds <- selected_table %>%
      mutate(seed = as.character(seed)) %>%
      pull((seed))
    
    if (length(selected_seeds) >= 1){
      selected_seeds
    } else {
      NULL
    }
  })
  
  filtredSeedsBas_plotly <- reactive({
    req(sim$seeds)
    
    if (is.null(input$plotly_brushed_bas)){
      return(list())
    }
    
    expressions_filtres <- expression_from_input(input$plotly_brushed_bas)
    colonnes_filtres <- parametres_from_input(input$plotly_brushed_bas)
    
    if ("sim_name" %in% colonnes_filtres){
      simNames <- NULL
    } else {
      simNames <- isolate(selected_experiments_debounced()) %>%
        as_tibble() %>%
        set_colnames("sim_name") %>%
        gather(parametre, valeurs) %>%
        distinct() %>%
        arrange(parametre, valeurs)
    }
    
    selected_table <- parameters_data() %>%
      mutate(seed = as.numeric(seed)) %>%
      mutate_if(is.character, funs(char = char_to_num)) %>%
      filter(!!!expressions_filtres)
    
    if (length(colonnes_filtres) > 0){
      tablesParams$bas <- selected_table %>%
        select(!!!colonnes_filtres) %>%
        gather(key = parametre, value = valeurs) %>%
        arrange(parametre, valeurs) %>%
        bind_rows(simNames) %>%
        distinct(parametre, valeurs) %>%
        group_by(parametre) %>%
        nest() %>%
        mutate(valeurs = map_chr(data, ~ unlist(.x, use.names = FALSE) %>% paste(., collapse = " ; "))) %>%
        select(parametre, valeurs)
    } else {
      tablesParams$bas <- simNames %>%
        group_by(parametre) %>%
        nest() %>%
        mutate(valeurs = map_chr(data, ~ unlist(.x, use.names = FALSE) %>% paste(., collapse = " ; "))) %>%
        select(parametre, valeurs)
    }
    
    
    selected_seeds <- selected_table %>%
      mutate(seed = as.character(seed)) %>%
      pull((seed))
    
    if (length(selected_seeds) >= 1){
      selected_seeds
    } else {
      NULL
    }
  })

  
  observe({
    
    if (length(filtredSeedsHaut_plotly()) > 0) {
      brushedSeeds <- tibble(seed = filtredSeedsHaut_plotly())
      for (df in names(filtredHaut)){
        filtredHaut[[df]] <- sim[[df]] %>% filter(seed %in% brushedSeeds$seed)
      }
    } else {
      for (df in names(filtredHaut)){
        filtredHaut[[df]] <- sim[[df]]
      }
    }
  })
  
  observe({

    if (length(filtredSeedsBas_plotly()) > 0) {
      brushedSeeds <- tibble(seed = filtredSeedsBas_plotly())
      for (df in names(filtredBas)){
        filtredBas[[df]] <- sim[[df]] %>% filter(seed %in% brushedSeeds$seed)
      }
    } else {
      for (df in names(filtredBas)){
        filtredBas[[df]] <- sim[[df]]
      }
    }
  })
  
  
  observe({
    req(tablesParams$haut)
    params <- tablesParams$haut
    
    if (nrow(params) > 1){
      selection <- params %>%
        purrr::transpose() %>%
        purrr::map(~unlist(.x)) %>%
        purrr::map(~paste0(.x, collapse = " : [")) %>%
        paste0(., collapse = "]\n")
    } else {
      selection <- params %>%
        purrr::transpose() %>%
        purrr::map(~paste0(.x, collapse = " : [")) %>%
        paste0(., "]")
    }
    tablesParams$hautTxt <-  selection
  })
  
  observe({
    req(tablesParams$bas)
    params <- tablesParams$bas
    
    if (nrow(params) > 1){
      selection <- params %>%
        purrr::transpose() %>%
        purrr::map(~unlist(.x)) %>%
        purrr::map(~paste0(.x, collapse = " : [")) %>%
        paste0(., collapse = "]\n")
    } else {
      selection <- params %>%
        purrr::transpose() %>%
        purrr::map(~paste0(.x, collapse = " : [")) %>%
        paste0(., "]")
    }
    tablesParams$basTxt <-  selection
  })

  
  # Debug module
  #
  # callModule(plotDownloadRate, "testModule",
  #            plotFun = reactive(Agregats_Nb(filtredHaut$agregats)),
  #            plotName = "Test1")
  
  # ---------------- Source all plots -----------------
  source("src_plots/plotly_helpers.R", local = TRUE, encoding = "utf8")
  source("src_plots/global_plots.R", local = TRUE, encoding = "utf8")
  source("src_plots/FP.R", local = TRUE, encoding = 'utf8')
  source("src_plots/Agregats.R", local = TRUE, encoding = 'utf8')
  source("src_plots/Seigneurs.R", local = TRUE, encoding = 'utf8')
  source("src_plots/Poles.R", local = TRUE, encoding = 'utf8')
  source("src_plots/Paroisses.R", local = TRUE, encoding = 'utf8')
  source("src_plots/sensitivity.R", local = TRUE, encoding = "utf8")
  
  # ---------------- Disconnect onSessionEnded -----------------
  # Cf. https://shiny.rstudio.com/reference/shiny/latest/onStop.html
  onStop(function(){
    dbDisconnect(conMapD)
    }
  )

  
})
