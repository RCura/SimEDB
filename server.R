library(shiny)
# options(warn=1)
# options(shiny.error = browser)

shinyServer(function(input, output, session) {
  
  setBookmarkExclude(c(
    "bookmarks",
    ".clientValue-default-plotlyCrosstalkOpts",
    "plotly_afterplot-parcoords_bas", 
    "plotly_afterplot-parcoords_haut",
    "plotHeight", "plotWidth",
    "show_resultsPlot",
    "selectedSims-selectized",
    "plotly_brushed_haut", "plotly_brushed_bas"
    ))
  
  # Connection to DB system (for MapD at least)
  # drv declared in global
  conMapD <- dbConnect(omnisci_driver, "jdbc:omnisci:mapdi.cura.info:6274:omnisci", "admin", "HyperInteractive")

  seeds <- tbl(conMapD, "seeds_6_4")
  agregats <- tbl(conMapD, "agregats_6_4")
  fp <- tbl(conMapD, "fp_6_4")
  parameters <- tbl(conMapD, "parameters_6_4")
  paroisses <- tbl(conMapD, "paroisses_6_4")
  poles <- tbl(conMapD, "poles_6_4")
  results <- tbl(conMapD, "global_6_4")
  seigneurs <- tbl(conMapD, "seigneurs_6_4")
  chateaux <- tbl(conMapD, "chateaux_6_4")
  
  
  # ---------------- Declare reactive values -----------------
  
  oldBrushedHaut <- reactiveVal(value = NA)
  oldBrushedBas <- reactiveVal(value = NA)
  
  bookmarks <- reactiveValues(constraintsHaut = NULL, constraintsBas = NULL)
  
  sim <- reactiveValues(agregats = agregats, FP =  fp, parameters = parameters,
                        paroisses = paroisses, poles = poles, results = results,
                        seigneurs = seigneurs, seeds = seeds, chateaux = chateaux)
  
  filtredHaut <- reactiveValues(agregats = NULL, FP = NULL, parameters = NULL,
                                paroisses = NULL, poles = NULL,
                                results = NULL, seigneurs = NULL, chateaux = NULL)
  
  filtredBas <- reactiveValues(agregats = NULL, FP = NULL, parameters = NULL,
                               paroisses = NULL, poles = NULL,
                               results = NULL, seigneurs = NULL, chateaux = NULL)
  
  tablesParams <- reactiveValues(haut = NULL, bas = NULL, hautTxt = NULL, basTxt = NULL)
  
  plotWidth <- reactive(input$plotWidth)
  plotHeight <- reactive(input$plotHeight)
  
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
    sim$chateaux <- chateaux %>% filter(sim_name %in% simNames)
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
  
  # Update filtredSeeds
  update_SimNames <- function(selected_expe){
    selected_expe %>%
      as_tibble() %>%
      set_colnames("sim_name") %>%
      gather(parametre, valeurs) %>%
      distinct() %>%
      arrange(parametre, valeurs)
  }
  
  filtredSeedsHaut_plotly <- reactive({
    req(sim$seeds)
    
    if (is.null(bookmarks$constraintsHaut)){
      return(list())
    }
    
    expressions_filtres <- expression_from_input(bookmarks$constraintsHaut)
    colonnes_filtres <- parametres_from_input(bookmarks$constraintsHaut)
    
    if ("sim_name" %in% colonnes_filtres){
      simNames <- NULL
    } else {
      simNames <- update_SimNames(isolate(selected_experiments_debounced()))
    }

    selected_table <- parameters_data() %>%
      select_at(vars("seed", !!!colonnes_filtres)) %>%
      mutate(variable_bidon = "1", variable_bidon2 = "2") %>% # Pour s'assurer qu'il y a toujours au moins 2 colonnes de param
      mutate_at(vars(everything(), -seed), list(char = char_to_num)) %>%
      select(-starts_with("variable_bidon")) %>% # On enlève les colonnes bidons
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

    if (is.null(bookmarks$constraintsBas)){
      return(list())
    }

    expressions_filtres <- expression_from_input(bookmarks$constraintsBas)
    colonnes_filtres <- parametres_from_input(bookmarks$constraintsBas)

    if ("sim_name" %in% colonnes_filtres){
      simNames <- NULL
    } else {
      simNames <- update_SimNames(isolate(selected_experiments_debounced()))
    }
    
    selected_table <- parameters_data() %>%
      select_at(vars("seed", !!!colonnes_filtres)) %>%
      mutate(variable_bidon = "1", variable_bidon2 = "2") %>% # Pour s'assurer qu'il y a toujours au moins 2 colonnes de param
      mutate_at(vars(everything(), -seed), list(char = char_to_num)) %>%
      select(-starts_with("variable_bidon")) %>% # On enlève les colonnes bidons
      filter(!!!expressions_filtres)

    if (length(colonnes_filtres) > 0){
      tablesParams$bas <- selected_table %>%
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
      brushedSeeds <- tibble(seed = filtredSeedsHaut_plotly()) %>%
        mutate(seed = if_else(condition = nchar(seed) < 10,
                              true = sprintf("%.7f", as.numeric(seed)),
                              false = seed))
      for (this_df in names(filtredHaut)){
        filtredHaut[[this_df]] <- sim[[this_df]] %>% filter(seed %in% local(brushedSeeds$seed))
      }
    } else {
      for (this_df in names(filtredHaut)){
        filtredHaut[[this_df]] <- sim[[this_df]]
      }
    }
  })
  
  observe({
    if (length(filtredSeedsBas_plotly()) > 0) {
      brushedSeeds <- tibble(seed = filtredSeedsBas_plotly()) %>%
        mutate(seed = if_else(condition = nchar(seed) < 10,
                              true = sprintf("%.7f", as.numeric(seed)),
                              false = seed))
      for (this_df in names(filtredBas)){
        filtredBas[[this_df]] <- sim[[this_df]] %>% filter(seed %in% local(brushedSeeds$seed))
      }
    } else {
      for (this_df in names(filtredBas)){
        filtredBas[[this_df]] <- sim[[this_df]]
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
        paste0(., collapse = "]\n") %>%
        paste0(., "]")
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
        paste0(., collapse = "]\n") %>%
        paste0(., "]")
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
  source("src_plots/Chateaux.R", local = TRUE, encoding = 'utf8')
  source("src_plots/sensitivity.R", local = TRUE, encoding = "utf8")
  
  # ---------------- Disconnect onSessionEnded -----------------
  # Cf. https://shiny.rstudio.com/reference/shiny/latest/onStop.html
  onStop(function(){
    dbDisconnect(conMapD)
    gc()
    }
  )
  
  
  observe({
    bookmarks$constraintsHaut <- input$plotly_brushed_haut
  })
  observe({
    bookmarks$constraintsBas <- input$plotly_brushed_bas
  })  
  
 observe({
   # Trigger this observer every time an input changes
   reactiveValuesToList(input)
   session$doBookmark()
 })
 onBookmarked(function(url) {
   updateQueryString(url)
 })

  onBookmark(session = session, fun = function(state) {
    state$values$constraintsHaut <- input$plotly_brushed_haut
    state$values$constraintsBas <- input$plotly_brushed_bas
  })
  
  onRestored(function(state){
    bookmarks$constraintsHaut <- state$values$constraintsHaut
    bookmarks$constraintsBas <- state$values$constraintsBas
  })

  
})
