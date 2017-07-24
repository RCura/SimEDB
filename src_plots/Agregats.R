Agregats_Nb <- function(agregats_data){
  nombre_agregats <- agregats_data %>%
    group_by(Annee, seed) %>%
    summarise(nb = n())
  
  ggplot(nombre_agregats, aes(factor(Annee), nb)) +
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
  txAgregatsPoles <- agregats_data %>%
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
    group_by(Annee, seed) %>%
    summarise(nb = n())
  
  ggplot(nombre_agregats, aes(factor(Annee), nb)) +
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
    group_by(seed, sim_name, Annee) %>%
    summarise(NbAgregats = n())
  
  agregatsParoisses <- poles_data %>%
    filter(!is.na(monAgregat)) %>%
    filter(nbParoisses >= 1) %>%
    group_by(sim_name, Annee, seed, monAgregat) %>%
    count() %>%
    group_by(Annee, seed, sim_name) %>%
    summarise(NbAgregatsParoisse = sum(n)) %>%
    right_join(nbAgregats, by = c("seed", "Annee", "sim_name")) %>%
    mutate(TxAgregatsParoisses = NbAgregatsParoisse / NbAgregats * 100) %>%
    gather(key = Type, value = Value, NbAgregatsParoisse, TxAgregatsParoisses) %>%
    mutate(Value = if_else(is.na(Value), 0, Value)) %>%
    mutate(Type = if_else(Type == "NbAgregatsParoisse", "Nombre", "Taux (en %)"))
  
  ggplot(agregatsParoisses, aes(factor(Annee), Value)) +
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