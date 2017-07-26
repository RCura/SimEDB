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
  Agregats_Nb(sim$agregats)
})

output$Agregats_Nb_Filter <- renderPlot({
  req(filtred$agregats)
  Agregats_Nb(filtred$agregats)
})

Agregats_Poles <- function(agregats_data){
  sansPoles <- agregats_data %>%
    filter(is.na(monpole)) %>%
    mutate(pole = FALSE)
  
  avecPoles <- agregats_data %>%
    filter(!is.na(monpole)) %>%
    mutate(pole = TRUE)
  
  nbAgregats <- agregats_data %>%
    group_by(seed, sim_name, annee) %>%
    summarise(nb_agregats = n())
  
  txAgregatsPoles <- sansPoles %>%
    union_all(avecPoles) %>%
    group_by(seed,annee, pole) %>%
    summarise(n = n()) %>%
    left_join(nbAgregats, by = c("seed", "annee")) %>%
    filter(pole == TRUE) %>%
    mutate(tx_agregat_pole = (n * 1.0) / (nb_agregats * 1.0)) %>%
    collect()
  
  
  ggplot(txAgregatsPoles, aes(annee, tx_agregat_pole, group = factor(annee))) +
    geom_tufteboxplot() +
    scale_y_continuous(labels = percent, limits = c(0,1)) +
    xlab("Temps") + ylab("Taux d'agrégats\n contenant un pôle") +
    ggtitle("Évolution du taux d'agrégats avec pôle") +
    labs(subtitle = "Variabilité : Réplications")
}

output$Agregats_Poles <- renderPlot({
  Agregats_Poles(agregats_data = sim$agregats)
})

output$Agregats_Poles_Filter <- renderPlot({
  req(filtred$agregats)
  Agregats_Poles(agregats_data = filtred$agregats)
})

Agregats_CA <- function(agregats_data){
  nombre_agregats <- agregats_data %>%
    filter(communaute) %>%
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
  Agregats_CA(agregats_data = sim$agregats)
})

output$Agregats_CA_Filter <- renderPlot({
  req(filtred$agregats)
  Agregats_CA(agregats_data = filtred$agregats)
})

Agregats_RT <- function(agregats_data){
  rtAgregats <- agregats_data %>%
    filter(annee %in% c(820, 940, 1040, 1160)) %>%
    group_by(seed, annee) %>%
    mutate(rank = min_rank(-nbfp)) %>%
    collect() %>%
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
  Agregats_RT(agregats_data = sim$agregats)
})

output$Agregats_RT_Filter <- renderPlot({
  req(filtred$agregats)
  Agregats_RT(agregats_data = filtred$agregats)
})


Agregats_Paroisses <- function(agregats_data, poles_data){
  nbAgregats <- agregats_data %>%
    group_by(seed, sim_name, annee) %>%
    summarise(nb_agregats = n())
  
  agregatsParoisses <- poles_data %>%
    filter(!is.na(monagregat)) %>%
    filter(nbparoisses >= 1) %>%
    group_by(sim_name, annee, seed, monagregat) %>%
    summarise(n = n()) %>%
    group_by(annee, seed, sim_name) %>%
    summarise(nb_agregats_paroisse = sum(n)) %>%
    right_join(nbAgregats, by = c("seed", "annee", "sim_name")) %>%
    mutate(tx_agregats_paroisses = (nb_agregats_paroisse * 1.0) / (nb_agregats * 1.0) * 100) %>%
    collect() %>%
    gather(key = Type, value = Value, nb_agregats_paroisse, tx_agregats_paroisses) %>%
    mutate(Value = if_else(is.na(Value), 0, Value)) %>%
    mutate(Type = if_else(Type == "nb_agregats_paroisse", "Nombre", "Taux (en %)"))
  
  ggplot(agregatsParoisses, aes(factor(annee), Value)) +
    geom_tufteboxplot() +
    facet_grid(Type~., scales = "free_y") +
    xlab("Temps") + ylab("Agrégats contenant au moins une paroisse") +
    ggtitle("Évolution du nombre d'agrégats contenant au moins une paroisse") +
    labs(subtitle = "Variabilité : Réplications")
}

output$Agregats_Paroisses <- renderPlot({
  Agregats_Paroisses(agregats_data = sim$agregats, poles_data = sim$poles)
})

output$Agregats_Paroisses_Filter <- renderPlot({
  req(filtred$poles, filtred$agregats)
  Agregats_Paroisses(agregats_data = filtred$agregats, poles_data = filtred$poles)
})