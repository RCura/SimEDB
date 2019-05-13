Paroisses_Nb <- function(paroisses_data){
  nombre_paroisses <- paroisses_data %>%
    group_by(annee, seed) %>%
    summarise(nb = n()) %>%
    collect()
  
  ggplot(nombre_paroisses, aes(factor(annee), nb)) +
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre de paroisses") +
    ggtitle("Évolution du nombre de paroisses") +
    labs(subtitle = "Variabilité : Réplications")
}

callModule(plotDownloadRate, paste0("Paroisses_Nb","_Haut"),
           plotFun = reactive(
             Paroisses_Nb(filtredHaut$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
             ),
           plotName = paste0("Paroisses_Nb","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Paroisses_Nb","_Bas"),
           plotFun = reactive(
             Paroisses_Nb(filtredBas$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
             ),
           plotName = paste0("Paroisses_Nb","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())


Paroisses_Compo <- function(paroisses_data){
  fidelesBreaks <- c(-1,0,10,30,50,100,1000)
  fidelesLabels <- c("0", "1-10", "11-30", "31-50", "51-100", ">100")
  
  paroisses_breaks <- paroisses_data %>%
    filter(annee %in% c(820, 960, 1060, 1200)) %>%
    collect() %>%
    mutate(NbFidelesBreaks = cut(nb_fideles, breaks = fidelesBreaks, labels = fidelesLabels)) %>%
    group_by(seed, annee, NbFidelesBreaks) %>%
    summarise(NbParoisses = n())
  
  ggplot(paroisses_breaks, aes(factor(NbFidelesBreaks), NbParoisses)) +
    geom_tufteboxplot() +
    facet_wrap(~annee, scales = "free") +
    xlab("Nombre de paroissiens") + ylab("Fréquence") +
    scale_x_discrete(drop = FALSE) +
    ggtitle("Evolution de la composition des paroisses") +
    labs(subtitle = "Variabilité : Réplications")
}

callModule(plotDownloadRate, paste0("Paroisses_Compo","_Haut"),
           plotFun = reactive(
             Paroisses_Compo(filtredHaut$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Paroisses_Compo","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Paroisses_Compo","_Bas"),
           plotFun = reactive(
             Paroisses_Compo(filtredBas$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
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
    xlab("Temps") + ylab("Nombre de nouvelles paroisses\nà chaque pas de temps") +
    ggtitle("Évolution des modes de création de nouvelles paroisses") +
    labs(subtitle = "Variabilité : Réplications")
}

callModule(plotDownloadRate, paste0("Paroisses_Promo","_Haut"),
           plotFun = reactive(
             Paroisses_Promo(filtredHaut$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Paroisses_Promo","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Paroisses_Promo","_Bas"),
           plotFun = reactive(
             Paroisses_Promo(filtredBas$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Paroisses_Promo","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())


Paroisses_Superficie <- function(paroisses_data){
  superficieBreaks <- c(-1,1, 5, 10, 20,50, 100,500, 1E12)
  superficieBreaks <- superficieBreaks * 1E6
  superficieLabels <- c("<1", "1-5", "6-10", "11-20", "21-50", "51-100", "101-500", ">500")
  
  paroisses_sup_breaks <-  paroisses_data %>%
    filter(annee %in% c(820, 960, 1060, 1200)) %>%
    collect() %>%
    mutate(NbSuperficiesBreaks = cut(superficie,
                                     breaks = superficieBreaks,
                                     labels = superficieLabels)) %>%
    group_by(seed, annee, NbSuperficiesBreaks) %>%
    summarise(NbParoisses = n())
  
  
  ggplot(paroisses_sup_breaks, aes(factor(NbSuperficiesBreaks), NbParoisses)) +
    geom_tufteboxplot() +
    facet_wrap(~annee) +
    xlab("Aire d'attraction des églises paroissiales (km²)") + ylab("Fréquence") +
    ggtitle("Évolution de l'aire d'attraction des églises paroissiales") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(subtitle = "Variabilité : Réplications")
}

callModule(plotDownloadRate, paste0("Paroisses_Superficie","_Haut"),
           plotFun = reactive(
             Paroisses_Superficie(filtredHaut$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Paroisses_Superficie","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Paroisses_Superficie","_Bas"),
           plotFun = reactive(
             Paroisses_Superficie(filtredBas$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Paroisses_Superficie","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())