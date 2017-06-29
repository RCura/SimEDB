output$polesNb <- renderPlot({
  PolesTous <- sim_poles %>%
    group_by(seed, Annee) %>%
    summarise(NbPoles = n()) %>%
    tbl_df()
  
  ggplot(data = PolesTous, aes(factor(Annee), NbPoles)) + 
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre de\npôles") +
    ggtitle("Évolution du nombre de pôles") +
    labs(subtitle = "Variabilité : Réplications")
  
})

output$polesNbFilter <- renderPlot({
  req(filtred$poles)
  PolesTous <- filtred$poles %>%
    group_by(seed, Annee) %>%
    summarise(NbPoles = n()) %>%
    tbl_df()
  
  ggplot(data = PolesTous, aes(factor(Annee), NbPoles)) + 
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre de\npôles") +
    ggtitle("Évolution du nombre de pôles") +
    labs(subtitle = "Variabilité : Réplications")
  
})

output$polesAgregats <- renderPlot({
  
  PolesTous <- sim_poles %>%
    group_by(seed, Annee) %>%
    summarise(NbPoles = n()) %>%
    tbl_df()
  
  tempVar <- sim_poles %>%
    filter(monAgregat != "nil") %>%
    group_by(seed,Annee) %>%
    summarise(NbPoleAg = n())
  
  PolesAgregats <- PolesTous %>%
    left_join(tempVar, by=c("seed", "Annee")) %>%
    mutate(TxAg = NbPoleAg / NbPoles)
  
  tousPoles <- ggplot(data = PolesAgregats, aes(factor(Annee), NbPoleAg)) + 
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre de pôles\nlocalisés dans un agrégat") +
    theme(axis.title.x=element_blank())
  
  polesAgregats <- ggplot(data = PolesAgregats, aes(factor(Annee), TxAg)) +
    geom_tufteboxplot() +
    scale_y_continuous(labels = scales::percent) +
    xlab("Temps") + ylab("Taux de pôles\nlocalisés dans un agrégat") +
    theme(axis.title.x=element_blank())
  
  grid.arrange(tousPoles, polesAgregats, bottom = 'Temps',
               top = "Évolution de la localisation des pôles
               Variabilité : Réplications")
  
})

output$polesAgregatsFilter <- renderPlot({
  req(filtred$poles)
  PolesTous <- filtred$poles %>%
    group_by(seed, Annee) %>%
    summarise(NbPoles = n()) %>%
    tbl_df()
  
  tempVar <- filtred$poles %>%
    filter(monAgregat != "nil") %>%
    group_by(seed,Annee) %>%
    summarise(NbPoleAg = n())
  
  PolesAgregats <- PolesTous %>%
    left_join(tempVar, by=c("seed", "Annee")) %>%
    mutate(TxAg = NbPoleAg / NbPoles)
  
  tousPoles <- ggplot(data = PolesAgregats, aes(factor(Annee), NbPoleAg)) + 
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre de pôles\nlocalisés dans un agrégat") +
    theme(axis.title.x=element_blank())
  
  polesAgregats <- ggplot(data = PolesAgregats, aes(factor(Annee), TxAg)) +
    geom_tufteboxplot() +
    scale_y_continuous(labels = scales::percent) +
    xlab("Temps") + ylab("Taux de pôles\nlocalisés dans un agrégat") +
    theme(axis.title.x=element_blank())
  
  grid.arrange(tousPoles, polesAgregats, bottom = 'Temps',
               top = "Évolution de la localisation des pôles
               Variabilité : Réplications")
  
})

output$polesCompo <- renderPlot({
  poles_temps <- sim_poles %>%
    filter(Annee %in% c(820, 940, 1040, 1160))
  
  compoPoles <- poles_temps %>%
    group_by(seed, Annee, nbAttracteurs) %>%
    summarise(Nb = n()) %>%
    tbl_df()
  
  ggplot(compoPoles, aes(factor(nbAttracteurs), Nb)) +
    geom_tufteboxplot() +
    facet_wrap(~Annee, scales = "free", nrow = 1) +
    xlab("Nombre d'attracteurs") +
    ylab("Fréquence") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Évolution du nombre d'attracteurs des pôles") +
    labs(subtitle = "Variabilité : Réplications")

})

output$polesCompoFilter <- renderPlot({
  req(filtred$poles)
  
  poles_temps <- filtred$poles %>%
    filter(Annee %in% c(820, 940, 1040, 1160))
  
  compoPoles <- poles_temps %>%
    group_by(seed, Annee, nbAttracteurs) %>%
    summarise(Nb = n()) %>%
    tbl_df()

  ggplot(compoPoles, aes(factor(nbAttracteurs), Nb)) +
    geom_tufteboxplot() +
    facet_wrap(~Annee, scales = "free", nrow = 1) +
    xlab("Nombre d'attracteurs") +
    ylab("Fréquence") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Évolution du nombre d'attracteurs des pôles") +
    labs(subtitle = "Variabilité : Réplications")
})


output$polesAttrac <- renderPlot({
  poles_temps <- sim_poles %>%
    filter(Annee %in% c(820, 940, 1040, 1160))
  
  attracPoles <- poles_temps %>%
    group_by(seed, Annee,attractivite) %>%
    summarise(Nb = n()) %>%
    tbl_df()
  
  ggplot(attracPoles, aes(factor(attractivite), Nb)) +
    geom_tufteboxplot() + 
    facet_wrap(~Annee, scales = "free", nrow = 1) +
    xlab("Attractivité") +
    ylab("Fréquence") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Évolution de l'attractivité des pôles") +
    labs(subtitle = "Variabilité : Réplications")
})

output$polesAttracFilter <- renderPlot({
  req(filtred$poles)
  
  poles_temps <- filtred$poles %>%
    filter(Annee %in% c(820, 940, 1040, 1160))
  
  attracPoles <- poles_temps %>%
    group_by(seed, Annee, attractivite) %>%
    summarise(Nb = n()) %>%
    tbl_df()
  
  ggplot(attracPoles, aes(factor(attractivite), Nb)) +
    geom_tufteboxplot() + 
    facet_wrap(~Annee, scales = "free", nrow = 1) +
    xlab("Attractivité") +
    ylab("Fréquence") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Évolution de l'attractivité des pôles") +
    labs(subtitle = "Variabilité : Réplications")
})

output$polesRT <- renderPlot({
  
  rtPoles_data <- sim_poles %>%
    filter(Annee %in% c(820, 940, 1040, 1160)) %>%
    group_by(seed, Annee) %>%
    mutate(Rank = row_number(desc(nbAttracteurs))) %>%
    group_by(Annee, Rank) %>%
    summarise(Moyenne = mean(nbAttracteurs),
              Q1 = quantile(nbAttracteurs, probs = 0.25),
              Q3 = quantile(nbAttracteurs, probs = 0.75)) %>%
    gather(key = `Méthode d'agrégation`, value = Value, Moyenne:Q3) %>%
    tbl_df() %>%
    ungroup()
  
  ggplot(rtPoles_data, aes(Rank, Value,
                           group = `Méthode d'agrégation`,
                           colour = `Méthode d'agrégation`)) +
    geom_line(size = 0.3, linetype = "dotted") +
    geom_point(size = 0.3) +
    scale_color_manual(values = c("black", "red", "blue")) +
    facet_grid(~Annee, space = "free_x",  scales = "free_x") +
    scale_x_log10() + scale_y_log10() +
    ggtitle("Évolution rang-taille de la composition des pôles") +
    theme(legend.position = "bottom") +
    xlab("Rang (log10)") + ylab("Nombre d'attracteurs (log10)") +
    labs(subtitle = "Variabilité : Moyenne, Q1 et Q3 des Réplications")

})

output$polesRTFilter <- renderPlot({
  req(filtred$poles)
  
  rtPoles_data <- filtred$poles %>%
    filter(Annee %in% c(820, 940, 1040, 1160)) %>%
    group_by(seed, Annee) %>%
    mutate(Rank = row_number(desc(nbAttracteurs))) %>%
    group_by(Annee, Rank) %>%
    summarise(Moyenne = mean(nbAttracteurs),
              Q1 = quantile(nbAttracteurs, probs = 0.25),
              Q3 = quantile(nbAttracteurs, probs = 0.75)) %>%
    gather(key = `Méthode d'agrégation`, value = Value, Moyenne:Q3) %>%
    tbl_df() %>%
    ungroup()
  
  ggplot(rtPoles_data, aes(Rank, Value,
                           group = `Méthode d'agrégation`,
                           colour = `Méthode d'agrégation`)) +
    geom_line(size = 0.3, linetype = "dotted") +
    geom_point(size = 0.3) +
    scale_color_manual(values = c("black", "red", "blue")) +
    facet_grid(~Annee, space = "free_x",  scales = "free_x") +
    scale_x_log10() + scale_y_log10() +
    ggtitle("Évolution rang-taille de la composition des pôles") +
    theme(legend.position = "bottom") +
    xlab("Rang (log10)") + ylab("Nombre d'attracteurs (log10)") +
    labs(subtitle = "Variabilité : Moyenne, Q1 et Q3 des Réplications")
})

