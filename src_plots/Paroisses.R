Paroisses_Nb <- function(results_data){
  nombre_paroisses <- results_data %>%
    select(annee, seed, nb_eglises, nb_eglises_paroissiales) %>%
    mutate(nb_eglises_non_paroissiales = nb_eglises - nb_eglises_paroissiales) %>%
    select(-nb_eglises) %>%
    collect() %>%
    gather(key = Type, value = Nb, -annee, -seed) %>%
    mutate(Type = case_when(
      Type == "nb_eglises_non_paroissiales" ~ "Eglises non paroissiales",
      Type == "nb_eglises_paroissiales" ~ "Eglises paroissiales",
      TRUE ~ as.character(Type)
    ))
  
  ggplot(nombre_paroisses, aes(factor(annee), Nb)) +
    geom_tufteboxplot() +
    facet_wrap(~fct_rev(Type), ncol = 1, scales="free_y") + 
    xlab("Temps") + ylab("Nombre d'églises") +
    ggtitle("Évolution du nombre de paroisses") +
    labs(subtitle = "Variabilité : Réplications") +
    theme_simedb_rotate_x()
}

callModule(plotDownloadRate, paste0("Paroisses_Nb","_Haut"),
           plotFun = reactive(
             Paroisses_Nb(filtredHaut$results) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt))
             ),
           plotName = paste0("Paroisses_Nb","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Paroisses_Nb","_Bas"),
           plotFun = reactive(
             Paroisses_Nb(filtredBas$results) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
             ),
           plotName = paste0("Paroisses_Nb","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())


Paroisses_Compo <- function(paroisses_data){
  fidelesBreaks <- c(-1,0,10,30,50,100,1000, 10E3)
  fidelesLabels <- c("0", "1-10", "11-30", "31-50", "51-100", "101-1000", ">1000")
  
  paroisses_breaks <- paroisses_data %>%
    filter(annee %in% c(820, 900, 1000, 1100, 1200)) %>%
    select(seed, annee, nb_fideles) %>%
    collect() %>%
    mutate(NbFidelesBreaks = cut(nb_fideles, breaks = fidelesBreaks, labels = fidelesLabels)) %>%
    group_by(seed, annee, NbFidelesBreaks) %>%
    summarise(NbParoisses = n())
  
  ggplot(paroisses_breaks, aes(factor(NbFidelesBreaks), NbParoisses)) +
    geom_tufteboxplot() +
    facet_wrap(~annee, scales = "free_y") +
    xlab("Nombre de paroissiens") + ylab("Fréquence") +
    scale_x_discrete(drop = FALSE) +
    ggtitle("Évolution du nombre de foyers paysans par paroisse") +
    labs(subtitle = "Variabilité : Réplications") +
    theme_simedb_rotate_x()
}

callModule(plotDownloadRate, paste0("Paroisses_Compo","_Haut"),
           plotFun = reactive(
             Paroisses_Compo(filtredHaut$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt))
           ),
           plotName = paste0("Paroisses_Compo","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Paroisses_Compo","_Bas"),
           plotFun = reactive(
             Paroisses_Compo(filtredBas$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
           ),
           plotName = paste0("Paroisses_Compo","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())


Paroisses_Promo <- function(paroisses_data){
  paroisses_promo <- paroisses_data %>%
    filter(!(mode_promotion %in% c("nil", "initialisation"))) %>%
    group_by(seed, annee, mode_promotion) %>%
    summarise(nb = n()) %>%
    collect() %>%
    mutate(mode_promotion = case_when(
      mode_promotion == "creation agregat" ~ "Création dans un agrégat",
      mode_promotion == "creation isole" ~ "Création en zone peu dense",
      mode_promotion == "promotion isole" ~ "Promotion en zone peu dense")
    )
  
  ggplot(paroisses_promo, aes(factor(annee), nb)) +
    geom_tufteboxplot() +
    facet_wrap(~ mode_promotion, ncol = 1, scales = "free_y") +
    xlab("Temps") + ylab("Nouvelles paroisses") +
    ggtitle("Évolution des modes de création de nouvelles paroisses") +
    labs(subtitle = "Variabilité : Réplications") +
    theme_simedb_rotate_x()
}

callModule(plotDownloadRate, paste0("Paroisses_Promo","_Haut"),
           plotFun = reactive(
             Paroisses_Promo(filtredHaut$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt))
           ),
           plotName = paste0("Paroisses_Promo","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Paroisses_Promo","_Bas"),
           plotFun = reactive(
             Paroisses_Promo(filtredBas$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
           ),
           plotName = paste0("Paroisses_Promo","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())


Paroisses_Superficie <- function(paroisses_data){
  superficieBreaks <- c(-1,1, 5, 10, 20,50, 100,500, 1E12)
  superficieBreaks <- superficieBreaks * 1E6
  superficieLabels <- c("<1", "1-5", "6-10", "11-20", "21-50", "51-100", "101-500", ">500")
  
  paroisses_sup_breaks <-  paroisses_data %>%
    filter(annee %in% c(820, 900, 1000, 1100, 1200)) %>%
    collect() %>%
    mutate(NbSuperficiesBreaks = cut(superficie,
                                     breaks = superficieBreaks,
                                     labels = superficieLabels)) %>%
    group_by(seed, annee, NbSuperficiesBreaks) %>%
    summarise(NbParoisses = n())
  
  
  ggplot(paroisses_sup_breaks, aes(factor(NbSuperficiesBreaks), NbParoisses)) +
    geom_tufteboxplot() +
    facet_wrap(~annee) +
    xlab("Superficie de l'aire de desserte des églises paroissiales (km²)") + ylab("Fréquence") +
    ggtitle("Évolution de l'aire de desserte des églises paroissiales") +
    labs(subtitle = "Variabilité : Réplications") +
    theme_simedb_rotate_x()
}

callModule(plotDownloadRate, paste0("Paroisses_Superficie","_Haut"),
           plotFun = reactive(
             Paroisses_Superficie(filtredHaut$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt))
           ),
           plotName = paste0("Paroisses_Superficie","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Paroisses_Superficie","_Bas"),
           plotFun = reactive(
             Paroisses_Superficie(filtredBas$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
           ),
           plotName = paste0("Paroisses_Superficie","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())