Paroisses_Nb <- function(paroisses_data){
  nombre_paroisses <- paroisses_data %>%
    group_by(Annee, seed) %>%
    summarise(nb = n())
  
  ggplot(nombre_paroisses, aes(factor(Annee), nb)) +
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre de paroisses") +
    ggtitle("Évolution du nombre de paroisses") +
    labs(subtitle = "Variabilité : Réplications")
}

output$Paroisses_Nb <- renderPlot({
  Paroisses_Nb(paroisses_data = sim$paroisses)
})

output$Paroisses_Nb_Filter <- renderPlot({
  req(filtred$paroisses)
  Paroisses_Nb(paroisses_data = filtred$paroisses)
})

Paroisses_Compo <- function(paroisses_data){
  fidelesBreaks <- c(-1,0,10,30,50,100,1000)
  fidelesLabels <- c("0", "1-10", "11-30", "31-50", "51-100", ">100")
  
  paroisses_breaks <- paroisses_data %>%
    filter(Annee %in% c(820, 940, 1040, 1160)) %>%
    mutate(NbFidelesBreaks = cut(nbFideles, breaks = fidelesBreaks, labels = fidelesLabels)) %>%
    group_by(seed, Annee, NbFidelesBreaks) %>%
    summarise(NbParoisses = n())
  
  ggplot(paroisses_breaks, aes(factor(NbFidelesBreaks), NbParoisses)) +
    geom_tufteboxplot() +
    facet_wrap(~Annee, scales = "free") +
    xlab("Nombre de paroissiens") + ylab("Fréquence") +
    ggtitle("Evolution de la composition des paroisses") +
    labs(subtitle = "Variabilité : Réplications")
}

output$Paroisses_Compo <- renderPlot({
  Paroisses_Compo(paroisses_data = sim$paroisses)
})

output$Paroisses_Compo_Filter <- renderPlot({
  req(filtred$paroisses)
  Paroisses_Compo(paroisses_data = filtred$paroisses)
})

Paroisses_Promo <- function(paroisses_data){
  paroisses_promo <- paroisses_data %>%
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
    ggtitle("Évolution des modes de création de nouvelles paroisses") +
    labs(subtitle = "Variabilité : Réplications")
}

output$Paroisses_Promo <- renderPlot({
  Paroisses_Promo(paroisses_data = sim$paroisses)
})

output$Paroisses_Promo_Filter <- renderPlot({
  req(filtred$paroisses)
  Paroisses_Promo(paroisses_data = filtred$paroisses)
})

Paroisses_Superficie <- function(paroisses_data){
  superficieBreaks <- c(-1,1, 5, 10, 20,50, 100,500, 1E12)
  superficieBreaks <- superficieBreaks * 1E6
  superficieLabels <- c("<1", "1-5", "6-10", "11-20", "21-50", "51-100", "101-500", ">500")
  
  paroisses_sup_breaks <-  paroisses_data %>%
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
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(subtitle = "Variabilité : Réplications")
}

output$Paroisses_Superficie <- renderPlot({
  Paroisses_Superficie(paroisses_data = sim$paroisses)
})

output$Paroisses_Superficie_Filter <- renderPlot({
  req(filtred$paroisses)
  Paroisses_Superficie(paroisses_data = filtred$paroisses)
})