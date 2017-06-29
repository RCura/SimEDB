output$Agregats_Nb <- renderPlot({
  
  nombre_agregats <- sim_agregats %>%
    group_by(Annee, seed) %>%
    summarise(nb = n())
  
  ggplot(nombre_agregats, aes(factor(Annee), nb)) +
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre d'agrégats") +
    ggtitle("Évolution du nombre d'agrégats") +
    labs(subtitle = "Variabilité : Réplications")
})

output$Agregats_Nb_Filter <- renderPlot({
  req(filtred$agregats)
  nombre_agregats <- filtred$agregats %>%
    group_by(Annee, seed) %>%
    summarise(nb = n())
  
  ggplot(nombre_agregats, aes(factor(Annee), nb)) +
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre d'agrégats") +
    ggtitle("Évolution du nombre d'agrégats") +
    labs(subtitle = "Variabilité : Réplications")
})

output$Agregats_Poles <- renderPlot({
txAgregatsPoles <- sim_agregats %>%
  mutate(pole = if_else(monPole == "nil", FALSE, TRUE)) %>%
  group_by(seed, Annee, pole) %>%
  summarise(N = n()) %>%
  group_by(seed, Annee) %>%
  mutate(NbAgregats = sum(N)) %>%
  filter(pole == TRUE) %>%
  mutate(TxAgregatPole = N / NbAgregats)


ggplot(txAgregatsPoles, aes(Annee, TxAgregatPole, group = factor(Annee))) +
  geom_tufteboxplot() +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  xlab("Temps") + ylab("Taux d'agrégats\n contenant un pôle") +
  ggtitle("Évolution du taux d'agrégats avec pôle") +
  labs(subtitle = "Variabilité : Réplications")
})

output$Agregats_Poles_Filter <- renderPlot({
  req(filtred$agregats)
  txAgregatsPoles <- filtred$agregats %>%
    mutate(pole = if_else(monPole == "nil", FALSE, TRUE)) %>%
    group_by(seed, Annee, pole) %>%
    summarise(N = n()) %>%
    group_by(seed, Annee) %>%
    mutate(NbAgregats = sum(N)) %>%
    filter(pole == TRUE) %>%
    mutate(TxAgregatPole = N / NbAgregats)
  
  
  ggplot(txAgregatsPoles, aes(Annee, TxAgregatPole, group = factor(Annee))) +
    geom_tufteboxplot() +
    scale_y_continuous(labels = percent, limits = c(0,1)) +
    xlab("Temps") + ylab("Taux d'agrégats\n contenant un pôle") +
    ggtitle("Évolution du taux d'agrégats avec pôle") +
    labs(subtitle = "Variabilité : Réplications")
})

output$Agregats_CA <- renderPlot({
  
  nombre_agregats <- sim_agregats %>%
    filter(communaute == "true") %>%
    group_by(Annee, seed) %>%
    summarise(nb = n())
  
  ggplot(nombre_agregats, aes(factor(Annee), nb)) +
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre d'agrégats") +
    ggtitle("Évolution du nombre d'agrégats ayant une CA") +
    labs(subtitle = "Variabilité : Réplications")
})

output$Agregats_CA_Filter <- renderPlot({
  req(filtred$agregats)
  nombre_agregats <- filtred$agregats %>%
    filter(communaute == "true") %>%
    group_by(Annee, seed) %>% summarise(nb = n())
  
  ggplot(nombre_agregats, aes(factor(Annee), nb)) +
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre d'agrégats") +
    ggtitle("Évolution du nombre d'agrégats ayant une CA") +
    labs(subtitle = "Variabilité : Réplications")
})

output$Agregats_RT <- renderPlot({
  rtAgregats <- sim_agregats %>%
    filter(Annee %in% c(820, 940, 1040, 1160)) %>%
    group_by(seed, Annee) %>%
    mutate(Rank = row_number(desc(nbFP))) %>%
    group_by(Annee, Rank) %>%
    summarise(Moyenne = mean(nbFP),
              Q1 = quantile(nbFP, probs = 0.25),
              Q3 = quantile(nbFP, probs = 0.75)) %>%
    gather(key = `Méthode d'agrégation`, value = Value, Moyenne:Q3) %>%
    tbl_df() %>%
    ungroup() %>%
    mutate(Annee = Annee - 20)
  
  ggplot(rtAgregats, aes(Rank, Value, group = `Méthode d'agrégation`, colour = `Méthode d'agrégation`)) +
    geom_line(size = 0.3, linetype = "dotted") +
    geom_point(size = 0.3) +
    scale_color_manual(values = c("black", "red", "blue")) +
    facet_grid(~Annee, space = "free_x",  scales = "free_x") +
    scale_x_log10() + scale_y_log10() +
    ggtitle("Évolution rang-taille de la composition des agrégats") +
    theme(legend.position = "bottom") +
    xlab("Rang (log10)") + ylab("Nombre de FP\ncontenus (log10)") +
    labs(subtitle = "Variabilité : Moyenne, Q1 et Q3 des Réplications")
})

output$Agregats_RT_Filter <- renderPlot({
  req(filtred$agregats)
  rtAgregats <- filtred$agregats %>%
    filter(Annee %in% c(820, 940, 1040, 1160)) %>%
    group_by(seed, Annee) %>%
    mutate(Rank = row_number(desc(nbFP))) %>%
    group_by(Annee, Rank) %>%
    summarise(Moyenne = mean(nbFP),
              Q1 = quantile(nbFP, probs = 0.25),
              Q3 = quantile(nbFP, probs = 0.75)) %>%
    gather(key = `Méthode d'agrégation`, value = Value, Moyenne:Q3) %>%
    tbl_df() %>%
    ungroup() %>%
    mutate(Annee = Annee - 20)
  
  ggplot(rtAgregats, aes(Rank, Value, group = `Méthode d'agrégation`, colour = `Méthode d'agrégation`)) +
    geom_line(size = 0.3, linetype = "dotted") +
    geom_point(size = 0.3) +
    scale_color_manual(values = c("black", "red", "blue")) +
    facet_grid(~Annee, space = "free_x",  scales = "free_x") +
    scale_x_log10() + scale_y_log10() +
    ggtitle("Évolution rang-taille de la composition des agrégats") +
    theme(legend.position = "bottom") +
    xlab("Rang (log10)") + ylab("Nombre de FP\ncontenus (log10)") +
    labs(subtitle = "Variabilité : Moyenne, Q1 et Q3 des Réplications")
})