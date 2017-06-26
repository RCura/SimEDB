output$paroissesNb <- renderPlot({
  
  nombre_paroisses <- sim_paroisses %>%
    group_by(Annee, seed) %>%
    summarise(nb = n())
  
  ggplot(nombre_paroisses, aes(factor(Annee), nb)) +
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre de paroisses") +
    ggtitle("Évolution du nombre de paroisses")
})

output$paroissesNbFilter <- renderPlot({
  nombre_paroisses <- filtred$paroisses %>%
    group_by(Annee, seed) %>%
    summarise(nb = n())
  
  ggplot(nombre_paroisses, aes(factor(Annee), nb)) +
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre de paroisses") +
    ggtitle("Évolution du nombre de paroisses")
})

output$paroissesCompo <- renderPlot({
  fidelesBreaks <- c(-1,0,10,30,50,100,1000)
  fidelesLabels <- c("0", "1-10", "11-30", "31-50", "51-100", ">100")
  
  paroisses_breaks <- sim_paroisses %>%
    filter(Annee %in% c(820, 940, 1040, 1160)) %>%
    mutate(NbFidelesBreaks = cut(nbFideles, breaks = fidelesBreaks, labels = fidelesLabels)) %>%
    group_by(seed, Annee, NbFidelesBreaks) %>%
    summarise(NbParoisses = n())
  
  ggplot(paroisses_breaks, aes(factor(NbFidelesBreaks), NbParoisses)) +
    geom_tufteboxplot() +
    facet_wrap(~Annee, scales = "free") +
    xlab("Nombre de paroissiens") + ylab("Fréquence") +
    ggtitle("Evolution de la composition des paroisses")
})

output$paroissesCompoFilter <- renderPlot({
  fidelesBreaks <- c(-1,0,10,30,50,100,1000)
  fidelesLabels <- c("0", "1-10", "11-30", "31-50", "51-100", ">100")
  
  paroisses_breaks <- filtred$paroisses %>%
    filter(Annee %in% c(820, 940, 1040, 1160)) %>%
    mutate(NbFidelesBreaks = cut(nbFideles, breaks = fidelesBreaks, labels = fidelesLabels)) %>%
    group_by(seed, Annee, NbFidelesBreaks) %>%
    summarise(NbParoisses = n())
  
  ggplot(paroisses_breaks, aes(factor(NbFidelesBreaks), NbParoisses)) +
    geom_tufteboxplot() +
    facet_wrap(~Annee, scales = "free") +
    xlab("Nombre de paroissiens") + ylab("Fréquence") +
    ggtitle("Evolution de la composition des paroisses")
})

output$paroissesSuperficie <- renderPlot({
  
  superficieBreaks <- c(-1,1, 5, 10, 20,50, 100,500, 1E12)
  superficieBreaks <- superficieBreaks * 1E6
  superficieLabels <- c("<1", "1-5", "6-10", "11-20", "21-50", "51-100", "101-500", ">500")
  
  paroisses_sup_breaks <-  sim_paroisses %>%
    filter(Annee %in% c(820, 940, 1040, 1160)) %>%
    mutate(NbSuperficiesBreaks = cut(shape.area,
                                     breaks = superficieBreaks,
                                     labels = superficieLabels)) %>%
    group_by(seed, Annee, NbSuperficiesBreaks) %>%
    summarise(NbParoisses = n())
  
  
  ggplot(paroisses_sup_breaks, aes(factor(NbSuperficiesBreaks), NbParoisses)) +
    geom_tufteboxplot() + facet_wrap(~Annee) +
    xlab("Superficie des paroisses (km²)") + ylab("Fréquence") +
    ggtitle("Évolution de la superficie des paroisses") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
})

output$paroissesSuperficieFilter <- renderPlot({
  
  superficieBreaks <- c(-1,1, 5, 10, 20,50, 100,500, 1E12)
  superficieBreaks <- superficieBreaks * 1E6
  superficieLabels <- c("<1", "1-5", "6-10", "11-20", "21-50", "51-100", "101-500", ">500")
  
  paroisses_sup_breaks <-  filtred$paroisses %>%
    filter(Annee %in% c(820, 940, 1040, 1160)) %>%
    mutate(NbSuperficiesBreaks = cut(shape.area, breaks = superficieBreaks, labels = superficieLabels)) %>%
    group_by(seed, Annee, NbSuperficiesBreaks) %>%
    summarise(NbParoisses = n())
  
  
  ggplot(paroisses_sup_breaks, aes(factor(NbSuperficiesBreaks), NbParoisses)) +
    geom_tufteboxplot() + facet_wrap(~Annee) +
    xlab("Superficie des paroisses (km²)") + ylab("Fréquence") +
    ggtitle("Evolution de la superficie des paroisses") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
})

output$paroissesPromo <- renderPlot({
  paroisses_promo <- sim_paroisses %>%
    filter(mode_promotion != "nil") %>%
    filter(mode_promotion != "initialisation") %>%
    group_by(seed, Annee, mode_promotion) %>%
    summarise(N = n()) %>%
    mutate(mode_promotion = case_when(
      mode_promotion == "creation agregat" ~ "Création dans un agrégat",
      mode_promotion == "creation isole" ~ "Création en zone peu dense",
      mode_promotion == "promotion isole" ~ "Promotion en zone peu dense")
    )
  
  ggplot(paroisses_promo, aes(Annee, N, group = factor(Annee))) +
    geom_tufteboxplot() +
    facet_wrap(~ mode_promotion, ncol = 1) +
    xlab("Temps") + ylab("Nombre de nouvelles paroisses\nà chaque pas de temps") +
    ggtitle("Évolution des modes de création de nouvelles paroisses")
})

output$paroissesPromoFilter <- renderPlot({
  paroisses_promo <- filtred$paroisses %>%
    filter(mode_promotion != "nil") %>%
    filter(mode_promotion != "initialisation") %>%
    group_by(seed, Annee, mode_promotion) %>%
    summarise(N = n()) %>%
    mutate(mode_promotion = case_when(
      mode_promotion == "creation agregat" ~ "Création dans un agrégat",
      mode_promotion == "creation isole" ~ "Création en zone peu dense",
      mode_promotion == "promotion isole" ~ "Promotion en zone peu dense")
    )
  
  ggplot(paroisses_promo, aes(Annee, N, group = factor(Annee))) +
    geom_tufteboxplot() +
    facet_wrap(~ mode_promotion, ncol = 1) +
    xlab("Temps") + ylab("Nombre de nouvelles paroisses\nà chaque pas de temps") +
    ggtitle("Évolution des modes de création de nouvelles paroisses")
})