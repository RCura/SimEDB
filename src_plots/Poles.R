Poles_Nb <- function(poles_data){
  PolesTous <- poles_data %>%
    group_by(seed, annee) %>%
    summarise(nb_poles = n()) %>%
    collect()
  
  ggplot(data = PolesTous, aes(factor(annee), nb_poles)) + 
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre de\npôles") +
    ggtitle("Évolution du nombre de pôles") +
    labs(subtitle = "Variabilité : Réplications")
}

output$Poles_Nb <- renderPlot({
  Poles_Nb(poles_data = sim$poles)
})

output$Poles_Nb_Filter <- renderPlot({
  req(filtred$poles)
  Poles_Nb(poles_data = filtred$poles)
})

Poles_Agregats <- function(poles_data){
  PolesTous <- poles_data %>%
    group_by(seed, annee) %>%
    summarise(nb_poles = n())
  
  tempVar <- poles_data %>%
    filter(monagregat != "<NA>") %>%
    group_by(seed,annee) %>%
    summarise(nb_pole_ag = n())
  
  PolesAgregats <- PolesTous %>%
    left_join(tempVar, by=c("seed", "annee")) %>%
    mutate(tx_ag = (nb_pole_ag * 1.0) / (nb_poles * 1.0)) %>%
    collect()
  
  tousPoles <- ggplot(data = PolesAgregats, aes(factor(annee), nb_pole_ag)) + 
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre de pôles\nlocalisés dans un agrégat") +
    theme(axis.title.x=element_blank())
  
  polesAgregats <- ggplot(data = PolesAgregats, aes(factor(annee), tx_ag)) +
    geom_tufteboxplot() +
    scale_y_continuous(labels = scales::percent) +
    xlab("Temps") + ylab("Taux de pôles\nlocalisés dans un agrégat") +
    theme(axis.title.x=element_blank())
  
  grid.arrange(tousPoles, polesAgregats, bottom = 'Temps',
               top = "Évolution de la localisation des pôles
               Variabilité : Réplications")
}

output$Poles_Agregats <- renderPlot({
  Poles_Agregats(poles_data = sim$poles)
})

output$Poles_Agregats_Filter <- renderPlot({
  req(filtred$poles)
  Poles_Agregats(poles_data = filtred$poles)
})

Poles_Compo <- function(poles_data){
  compoPoles <- poles_data %>%
    filter(annee %in% c(820, 940, 1040, 1160)) %>%
    group_by(seed, annee, nbattracteurs) %>%
    summarise(nb = n()) %>%
    collect()
  
  ggplot(compoPoles, aes(factor(nbattracteurs), nb)) +
    geom_tufteboxplot() +
    facet_wrap(~annee, scales = "free", nrow = 1) +
    xlab("Nombre d'attracteurs") +
    ylab("Fréquence") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Évolution du nombre d'attracteurs des pôles") +
    labs(subtitle = "Variabilité : Réplications")
}

output$Poles_Compo <- renderPlot({
  Poles_Compo(poles_data = sim$poles)
})

output$Poles_Compo_Filter <- renderPlot({
  req(filtred$poles)
  Poles_Compo(poles_data = filtred$poles)
})

Poles_Attrac <- function(poles_data){
  attracPoles <- poles_data %>%
    filter(annee %in% c(820, 940, 1040, 1160)) %>%
    group_by(seed, annee, attractivite) %>%
    summarise(nb = n()) %>%
    collect()
  
  ggplot(attracPoles, aes(factor(attractivite), nb)) +
    geom_tufteboxplot() + 
    facet_wrap(~annee, scales = "free", nrow = 1) +
    xlab("Attractivité") +
    ylab("Fréquence") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Évolution de l'attractivité des pôles") +
    labs(subtitle = "Variabilité : Réplications")
}

output$Poles_Attrac <- renderPlot({
  Poles_Attrac(poles_data = sim$poles)
})

output$Poles_Attrac_Filter <- renderPlot({
  req(filtred$poles)
  Poles_Attrac(poles_data = filtred$poles)
})

Poles_RT <- function(poles_data){
  rtPoles_data <- poles_data %>%
    filter(annee %in% c(820, 940, 1040, 1160)) %>%
    group_by(seed, annee) %>%
    mutate(rank = min_rank(-nbattracteurs)) %>%
    group_by(annee, rank) %>%
    collect() %>%
    summarise(Moyenne = mean(nbattracteurs),
              Q1 = quantile(nbattracteurs, probs = 0.25),
              Q3 = quantile(nbattracteurs, probs = 0.75)) %>%
    gather(key = `Méthode d'agrégation`, value = Value, Moyenne:Q3) %>%
    ungroup()
  
  ggplot(rtPoles_data, aes(rank, Value,
                           group = `Méthode d'agrégation`,
                           colour = `Méthode d'agrégation`)) +
    geom_line(size = 0.3, linetype = "dotted") +
    geom_point(size = 0.3) +
    scale_color_manual(values = c("black", "red", "blue")) +
    facet_grid(~annee, space = "free_x",  scales = "free_x") +
    scale_x_log10() + scale_y_log10() +
    ggtitle("Évolution rang-taille de la composition des pôles") +
    theme(legend.position = "bottom") +
    xlab("Rang (log10)") + ylab("Nombre d'attracteurs (log10)") +
    labs(subtitle = "Variabilité : Moyenne, Q1 et Q3 des Réplications")
}

output$Poles_RT <- renderPlot({
  Poles_RT(poles_data = sim$poles)
})

output$Poles_RT_Filter <- renderPlot({
  req(filtred$poles)
  Poles_RT(poles_data = filtred$poles)
})