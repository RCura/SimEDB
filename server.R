library(shiny)

shinyServer(function(session, input, output) {
  
  # Connection to DB system (for MapD at least)
  # drv declared in global
  conMapD <- dbConnect(drv, "jdbc:mapd:mapdi.cura.info:9091:mapd", "mapd", "HyperInteractive")
  seeds <- tbl(conMapD, "seeds")
  agregats <- tbl(conMapD, "agregats")
  fp <- tbl(conMapD, "fp")
  parameters <- tbl(conMapD, "parameters")
  paroisses <- tbl(conMapD, "paroisses")
  poles <- tbl(conMapD, "poles")
  results <- tbl(conMapD, "results")
  seigneurs <- tbl(conMapD, "seigneurs")
  
  
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
  
  # ---------------- Parallel Coordinates Filtering -----------------

  
  filtredSeedsHaut_plotly <- reactive({
    req(sim$seeds)

    selected_seeds <- parameters_data() %>%
      mutate(seed = as.numeric(seed)) %>%
      mutate_if(is.character, funs(char_to_num)) %>%
      filter(!!!(expression_from_input(input$plotly_brushed_haut))) %>%
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
    
    selected_seeds <- parameters_data() %>%
      mutate(seed = as.numeric(seed)) %>%
      mutate_if(is.character, funs(char_to_num)) %>%
      filter(!!!(expression_from_input(input$plotly_brushed_bas))) %>%
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
        # For MonetDB
        # filtredHaut[[df]] <- sim[[df]] %>% inner_join(brushedSeeds, by = "seed", copy = TRUE)
        # For MapD
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
        # For MonetDB
        # filtredBas[[df]] <- sim[[df]] %>% inner_join(brushedSeeds, by = "seed", copy = TRUE)
        # For MapD
        filtredBas[[df]] <- sim[[df]] %>% filter(seed %in% brushedSeeds$seed)
      }
      
    } else {
      for (df in names(filtredBas)){
        filtredBas[[df]] <- NULL
      }
    }
  })
  
  # ---------------- Source all plots -----------------
  source("src_plots/plotly_helpers.R", local = TRUE, encoding = "utf8")
  source("src_plots/global_plots.R", local = TRUE, encoding = "utf8")
  source("src_plots/FP.R", local = TRUE, encoding = 'utf8')
  source("src_plots/Agregats.R", local = TRUE, encoding = 'utf8')
  source("src_plots/Seigneurs.R", local = TRUE, encoding = 'utf8')
  source("src_plots/Poles.R", local = TRUE, encoding = 'utf8')
  source("src_plots/Paroisses.R", local = TRUE, encoding = 'utf8')
  source("src_plots/download_plots.R", local = TRUE, encoding = 'utf8')
  source("src_plots/rating.R", local = TRUE, encoding = "utf8")
  source("src_plots/sensitivity.R", local = TRUE, encoding = "utf8")
  
  # ---------------- Disconnect onSessionEnded -----------------
  # Cf. https://shiny.rstudio.com/reference/shiny/latest/onStop.html
  onStop(function(){
    dbDisconnect(conMapD)
    }
  )

  
})
