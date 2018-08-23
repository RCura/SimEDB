Agregats_Nb <- function(agregats_data){
  nombre_agregats <- agregats_data %>%
    group_by(annee, seed) %>%
    summarise(nb = n()) %>%
    collect()
  
  ggplot(nombre_agregats, aes(factor(annee), nb)) +
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre d'agrégats") +
    ggtitle("Évolution du nombre d'agrégats") +
    labs(subtitle = "Variabilité : Réplications")
}

output$Agregats_Nb <- renderPlot({
  req(filtredHaut$agregats)
  Agregats_Nb(filtredHaut$agregats)
})

output$Agregats_Nb_Filter <- renderPlot({
  req(filtredBas$agregats)
  Agregats_Nb(filtredBas$agregats)
})

Agregats_Poles <- function(agregats_data){
  
  nbAgregats <- agregats_data %>%
    group_by(seed, sim_name, annee) %>%
    summarise(nb_agregats = n())
  
  avecPoles <- agregats_data %>%
    filter(!is.na(monpole)) %>%
    mutate(pole = TRUE) %>%
    group_by(seed, annee) %>%
    summarise(nb_poles = n())
  
  txAgregatsPoles <- nbAgregats %>%
    left_join(avecPoles, by = c("seed", "annee")) %>%
    mutate(tx_agregat_pole = (nb_poles + 1E-12) / (nb_agregats + 1E-12)) %>%
    collect()
  
  
  ggplot(txAgregatsPoles, aes(factor(annee), tx_agregat_pole)) +
    geom_tufteboxplot() +
    scale_y_continuous(labels = percent, limits = c(0,1)) +
    xlab("Temps") + ylab("Taux d'agrégats\n contenant un pôle") +
    ggtitle("Évolution du taux d'agrégats avec pôle") +
    labs(subtitle = "Variabilité : Réplications")
}

output$Agregats_Poles <- renderPlot({
  req(filtredHaut$agregats)
  Agregats_Poles(agregats_data = filtredHaut$agregats)
})

output$Agregats_Poles_Filter <- renderPlot({
  req(filtredBas$agregats)
  Agregats_Poles(agregats_data = filtredBas$agregats)
})

Agregats_CA <- function(agregats_data){
  
  nombre_agregats <- agregats_data %>%
    filter(communaute == 1) %>%
    group_by(annee, seed) %>%
    summarise(nb = n()) %>%
    collect()
  
  ggplot(nombre_agregats, aes(factor(annee), nb)) +
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre d'agrégats") +
    ggtitle("Évolution du nombre d'agrégats ayant une CA") +
    labs(subtitle = "Variabilité : Réplications")
}

output$Agregats_CA <- renderPlot({
  req(filtredHaut$agregats)
  Agregats_CA(agregats_data = filtredHaut$agregats)
})

output$Agregats_CA_Filter <- renderPlot({
  req(filtredBas$agregats)
  Agregats_CA(agregats_data = filtredBas$agregats)
})

Agregats_RT <- function(agregats_data){
  
  rtAgregats <- agregats_data %>%
    filter(annee %in% c(820, 940, 1040, 1160)) %>%
    collect() %>%
    group_by(seed, annee) %>%
    mutate(rank = min_rank(-nbfp)) %>%
    group_by(annee, rank) %>%
    summarise(Moyenne = mean(nbfp, na.rm = TRUE),
              Q1 = quantile(nbfp, probs = 0.25),
              Q3 = quantile(nbfp, probs = 0.75)) %>%
    gather(key = `Méthode d'agrégation`, value = Value, Moyenne:Q3) %>%
    ungroup()
  
  ggplot(rtAgregats, aes(rank, Value, group = `Méthode d'agrégation`, colour = `Méthode d'agrégation`)) +
    geom_line(size = 0.3, linetype = "dotted") +
    geom_point(size = 0.3) +
    scale_color_manual(values = c("black", "red", "blue")) +
    facet_grid(~annee, space = "free_x",  scales = "free_x") +
    scale_x_log10() + scale_y_log10() +
    ggtitle("Évolution rang-taille de la composition des agrégats") +
    theme(legend.position = "bottom") +
    xlab("Rang (log10)") + ylab("Nombre de FP\ncontenus (log10)") +
    labs(subtitle = "Variabilité : Moyenne, Q1 et Q3 des Réplications")
}

output$Agregats_RT <- renderPlot({
  req(filtredHaut$agregats)
  Agregats_RT(agregats_data = filtredHaut$agregats)
})

output$Agregats_RT_Filter <- renderPlot({
  req(filtredBas$agregats)
  Agregats_RT(agregats_data = filtredBas$agregats)
})


Agregats_Paroisses <- function(agregats_data, poles_data){
  nb_agregats <- agregats_data %>%
    group_by(seed, sim_name, annee) %>%
    summarise(nb_agregats = n()) %>%
    ungroup()%>%
    collect()

  nb_agregats_paroisses <- agregats %>%
    select(id_agregat, sim_name, seed, annee, monpole) %>%
    left_join(poles %>%
                select(sim_name, seed, annee, id_pole, monagregat, nbparoisses),
              by = c("sim_name", "seed", "annee", "monpole" = "id_pole")) %>%
    filter(nbparoisses >= 1) %>%
    group_by(seed, sim_name, annee) %>%
    summarise(nb_agregats_paroisses = n()) %>%
    collect()
  
  plot_data <-  nb_agregats %>%
    left_join(nb_agregats_paroisses, by = c("seed", "sim_name", "annee")) %>%
    mutate(taux_agregats = nb_agregats_paroisses / nb_agregats * 100) %>%
    select(-nb_agregats) %>%
    gather(key = Type, value = Value, nb_agregats_paroisses, taux_agregats) %>%
    mutate(Type = if_else(Type == "nb_agregats_paroisses", "Nombre", "Taux (en %)"))

  ggplot(plot_data) +
    aes(annee, Value, group = factor(annee)) +
    geom_tufteboxplot() +
    facet_grid(Type~., scales = "free_y") +
    xlab("Temps") + ylab("Agrégats contenant au moins une paroisse") +
    ggtitle("Évolution du nombre d'agrégats contenant au moins une paroisse") +
    labs(subtitle = "Variabilité : Réplications")
}

output$Agregats_Paroisses <- renderPlot({
  req(filtredHaut$agregats,  filtredHaut$poles)
  Agregats_Paroisses(agregats_data = filtredHaut$agregats, poles_data = filtredHaut$poles)
})

output$Agregats_Paroisses_Filter <- renderPlot({
  req(filtredBas$poles, filtredBas$agregats)
  Agregats_Paroisses(agregats_data = filtredBas$agregats, poles_data = filtredBas$poles)
})