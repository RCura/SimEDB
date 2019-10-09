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
           seeds = filtredSeedsHaut_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)

callModule(plotDownloadRate, paste0("Paroisses_Nb","_Bas"),
           plotFun = reactive(
             Paroisses_Nb(filtredBas$results) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
             ),
           plotName = paste0("Paroisses_Nb","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)


Paroisses_Compo <- function(paroisses_data){
  fidelesBreaks <- c(-1,0,10,30,50,100,1000, 10E3)
  fidelesLabels <- c("0", "1-10", "11-30", "31-50", "51-100", "101-1000", ">1000")
  fidelesBreaks <- c(-1,0,50,100,200,300,500, 1000,10E3)
  fidelesLabels <- c("0", "1-50", "51-100", "101-200", "201-300", "301-500", "501-1000",">1000")
  
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
           seeds = filtredSeedsHaut_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)

callModule(plotDownloadRate, paste0("Paroisses_Compo","_Bas"),
           plotFun = reactive(
             Paroisses_Compo(filtredBas$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
           ),
           plotName = paste0("Paroisses_Compo","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)


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
           seeds = filtredSeedsHaut_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)

callModule(plotDownloadRate, paste0("Paroisses_Promo","_Bas"),
           plotFun = reactive(
             Paroisses_Promo(filtredBas$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
           ),
           plotName = paste0("Paroisses_Promo","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)

Paroisses_Promo_CumSum <- function(paroisses_data){
  paroisses_promo <- paroisses_data %>%
    filter(!(mode_promotion %in% c("nil", "initialisation"))) %>%
    group_by(seed, annee, mode_promotion) %>%
    summarise(nb = n()) %>%
    collect() %>%
    mutate(mode_promotion = case_when(
      mode_promotion == "creation agregat" ~ "Création dans un agrégat",
      mode_promotion == "creation isole" ~ "Création/Promotion en zone peu dense",
      mode_promotion == "promotion isole" ~ "Création/Promotion en zone peu dense")
    ) %>%
    group_by(seed, annee, mode_promotion) %>%
    summarise(sum_nb = sum(nb, na.rm = TRUE)) %>%
    ungroup() %>%
    complete(seed, annee, mode_promotion, fill = list(sum_nb = 0)) %>%
    arrange(seed, annee, mode_promotion) %>%
    group_by(seed, mode_promotion) %>%
    mutate(cumsum2 = cumsum(sum_nb)) %>%
    ungroup()
    
    
  
  ggplot(paroisses_promo, aes(factor(annee), cumsum2)) +
    geom_tufteboxplot() +
    facet_wrap(~ mode_promotion, ncol = 1, scales = "free_y") +
    xlab("Temps") + ylab("Nouvelles paroisses\n(Somme cumulée)") +
    ggtitle("Évolution des modes de création de nouvelles paroisses") +
    labs(subtitle = "Variabilité : Réplications") +
    theme_simedb_rotate_x()
}

callModule(plotDownloadRate, paste0("Paroisses_Promo_CumSum","_Haut"),
           plotFun = reactive(
             Paroisses_Promo_CumSum(filtredHaut$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt))
           ),
           plotName = paste0("Paroisses_Promo_CumSum","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)

callModule(plotDownloadRate, paste0("Paroisses_Promo_CumSum","_Bas"),
           plotFun = reactive(
             Paroisses_Promo_CumSum(filtredBas$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
           ),
           plotName = paste0("Paroisses_Promo_CumSum","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)


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
           seeds = filtredSeedsHaut_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)

callModule(plotDownloadRate, paste0("Paroisses_Superficie","_Bas"),
           plotFun = reactive(
             Paroisses_Superficie(filtredBas$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
           ),
           plotName = paste0("Paroisses_Superficie","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)


Paroisses_Carte <- function(paroisses_data){
  
  random_seeds <- paroisses_data %>%
    select(seed) %>%
    group_by(seed) %>%
    tally() %>%
    collect() %>%
    sample_n(size = 2) %>%
    pull(seed)
  
  random_seeds <- paroisses_data %>%
    select(seed) %>%
    group_by(seed) %>%
    tally() %>%
    head(2) %>%
    pull(seed)
  
  paroisses_choisies <- paroisses_data %>%
    filter(annee %in% c(820, 900, 1000, 1100, 1200)) %>%
    filter(seed %in% local(random_seeds)) %>%
    select(seed, annee, nb_fideles, geom) %>%
    collect() %>%
    st_as_sf(wkt = "geom") %>%
    mutate(surface_m2 = st_area(.)) %>%
    mutate(surface_km2 = surface_m2 / 1E6) %>%
    mutate(densite = nb_fideles / surface_km2) %>%
    rename(`Année` = annee) %>%
    arrange(seed) %>%
    group_by(seed) %>%
    mutate(Simulation = group_indices()) %>%
    ungroup() %>%
    mutate(densite_cut = case_when(
      densite < 1 ~ "<1",
      densite <= 10 ~ "1-10",
      densite <= 25 ~ "11-25",
      densite <= 50 ~ "26-50",
      densite <= 100 ~ "51-100",
      densite > 100~ ">100"
      )
    ) %>%
    mutate(densite_cut = factor(densite_cut, labels = c("<1", "1-10", "11-25", "26-50", "51-100", ">100")))
    
  
  ggplot(paroisses_choisies) +
    aes(fill = densite_cut) +
    geom_sf(colour = "black", size = 0.1) +
    scale_fill_brewer(name = "Densité\n(Foyers paysans / km²)", type = "seq") +
    guides(fill = guide_legend(nrow = 1)) +
    coord_sf() +
    facet_grid(Simulation~`Année`, labeller = label_both) +
    ggtitle("Densité de paroissiens au cours du temps") +
    xlab("") + ylab("") +
    labs(subtitle = "Variabilité : Aucune") +
    theme_simedb_map()
}

callModule(plotDownloadRate, paste0("Paroisses_Carte","_Haut"),
           plotFun = reactive(
             Paroisses_Carte(filtredHaut$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt))
           ),
           plotName = paste0("Paroisses_Carte","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)

callModule(plotDownloadRate, paste0("Paroisses_Carte","_Bas"),
           plotFun = reactive(
             Paroisses_Carte(filtredBas$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
           ),
           plotName = paste0("Paroisses_Carte","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)

Paroisses_RT <- function(paroisses_data){
  
  rtParoisses <- paroisses_data %>%
    filter(annee %in% c(820, 900, 1000, 1100, 1200)) %>%
    group_by(seed, annee) %>%
    mutate(rank = rank(desc(nb_fideles))) %>%
    ungroup() %>%
    collect() %>%
    group_by(annee, rank) %>%
    summarise(Moyenne = mean(nb_fideles, na.rm = TRUE),
              Q1 = quantile(nb_fideles, probs = 0.25),
              Q3 = quantile(nb_fideles, probs = 0.75)) %>%
    gather(key = `Méthode d'agrégation`, value = Value, Moyenne:Q3) %>%
    ungroup()
  
  ggplot(rtParoisses, aes(rank, Value, group = `Méthode d'agrégation`, colour = `Méthode d'agrégation`)) +
    geom_line(size = 0.3, linetype = "dotted") +
    geom_point(size = 0.3) +
    scale_color_manual(values = c("black", "red", "blue"), name = NULL) +
    facet_grid(~annee, space = "free_x",  scales = "free_x") +
    scale_x_log10() + scale_y_log10() +
    ggtitle("Évolution de la distribution rang-taille des paroisses") +
    xlab("Rang (log10)") + ylab("Nombre de paroissiens") +
    labs(subtitle = "Variabilité : Moyenne, Q1 et Q3 des Réplications") +
    theme_simedb()
}

callModule(plotDownloadRate, paste0("Paroisses_RT","_Haut"),
           plotFun = reactive(
             Paroisses_RT(filtredHaut$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt))
           ),
           plotName = paste0("Paroisses_RT","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)

callModule(plotDownloadRate, paste0("Paroisses_RT","_Bas"),
           plotFun = reactive(
             Paroisses_RT(filtredBas$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
           ),
           plotName = paste0("Paroisses_RT","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)


Paroisses_Desserte <- function(paroisses_data){
  
  nb_paroisses <- paroisses_data %>%
    tally() %>%
    collect() %>%
    pull(n)
  
  if (nb_paroisses > 200E3){
    stop(safeError(
      "Le nombre de simulations est trop important pour calculer cet indicateur."
    ))
  }
  
  random_seeds <- paroisses_data %>%
    select(seed) %>%
    group_by(seed) %>%
    tally() %>%
    head(1) %>%
    pull(seed)
  
  grille_monde <- paroisses_data %>%
    filter(seed %in% random_seeds) %>%
    filter(annee == 820) %>%
    select(id_paroisse, geom) %>%
    collect() %>%
    st_as_sf(wkt = "geom")%>%
    summarise() %>%
    st_make_grid(n = 20) %>%
    st_sf() %>%
    mutate(id = row_number())
  
  desserte_monde <- paroisses_data %>%
    select(seed, annee, geom) %>%
    collect() %>%
    st_as_sf(wkt = "geom") %>%
    st_set_agr("constant") %>% # pour supprimer le warning
    st_centroid() %>%
    st_join(grille_monde) %>%
    st_drop_geometry() %>%
    group_by(seed, annee, id) %>%
    tally() %>%
    group_by(seed, annee) %>%
    summarise(taux_carreaux = n() / nrow(grille_monde))
  
  ggplot(desserte_monde) +
    aes(factor(annee), taux_carreaux) +
    geom_tufteboxplot() + 
    scale_y_continuous(labels = scales::percent, limits = c(0,1), breaks = seq(0,1.0, by = 0.2),
                       minor_breaks = seq(0.1, 0.9, by = 0.2), expand = c(0,0)) +
    labs(
      x = "Temps",
      y= "Part du carroyage\ncontenant une église paroissiale",
      title = "Évolution de la desserte paroissiale",
      subtitle = "Le monde est discrétisé en 400 carreaux (20*20).\nOn représente la proportion de carreaux qui contiennent au moins une église paroissiale"
    ) +
    theme_simedb()
  
  # ggplot(rtParoisses, aes(rank, Value, group = `Méthode d'agrégation`, colour = `Méthode d'agrégation`)) +
  #   geom_line(size = 0.3, linetype = "dotted") +
  #   geom_point(size = 0.3) +
  #   scale_color_manual(values = c("black", "red", "blue"), name = NULL) +
  #   facet_grid(~annee, space = "free_x",  scales = "free_x") +
  #   scale_x_log10() + scale_y_log10() +
  #   ggtitle("Évolution de la distribution rang-taille des paroisses") +
  #   xlab("Rang (log10)") + ylab("Nombre de paroissiens") +
  #   labs(subtitle = "Variabilité : Moyenne, Q1 et Q3 des Réplications") +
  #   theme_simedb()
}

callModule(plotDownloadRate, paste0("Paroisses_Desserte","_Haut"),
           plotFun = reactive(
             Paroisses_Desserte(filtredHaut$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt))
           ),
           plotName = paste0("Paroisses_Desserte","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)

callModule(plotDownloadRate, paste0("Paroisses_Desserte","_Bas"),
           plotFun = reactive(
             Paroisses_Desserte(filtredBas$paroisses) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
           ),
           plotName = paste0("Paroisses_Desserte","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)