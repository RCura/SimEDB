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

callModule(plotDownloadRate, paste0("Agregats_Nb","_Haut"),
           plotFun = reactive(
             Agregats_Nb(filtredHaut$agregats) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Agregats_Nb","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())
callModule(plotDownloadRate, paste0("Agregats_Nb","_Bas"),
           plotFun = reactive(
             Agregats_Nb(filtredBas$agregats) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Agregats_Nb","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())

Agregats_Poles <- function(agregats_data){
  
  nbAgregats <- agregats_data %>%
    group_by(seed, sim_name, annee) %>%
    summarise(nb_agregats = n())
  
  avecPoles <- agregats_data %>%
    filter(monpole > 0) %>%
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

callModule(plotDownloadRate, paste0("Agregats_Poles","_Haut"),
           plotFun = reactive(
             Agregats_Poles(filtredHaut$agregats) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Agregats_Poles","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())
callModule(plotDownloadRate, paste0("Agregats_Poles","_Bas"),
           plotFun = reactive(
             Agregats_Poles(filtredBas$agregats) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Agregats_Poles","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())

Agregats_Paroisses <- function(agregats_data, poles_data){
  nb_agregats <- agregats_data %>%
    group_by(seed, sim_name, annee) %>%
    summarise(nb_agregats = n()) %>%
    ungroup()%>%
    collect()
  
  nb_agregats_paroisses <- agregats %>%
    select(id_agregat, sim_name, seed, annee, monpole) %>%
    left_join(poles_data %>%
                select(sim_name, seed, annee, id_pole, monagregat, nb_paroisses),
              by = c("sim_name", "seed", "annee", "monpole" = "id_pole")) %>%
    filter(nb_paroisses >= 1) %>%
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
    aes(factor(annee), Value) +
    geom_tufteboxplot() +
    facet_grid(Type~., scales = "free_y") +
    xlab("Temps") + ylab("Agrégats contenant au moins une paroisse") +
    ggtitle("Évolution du nombre d'agrégats contenant au moins une paroisse") +
    labs(subtitle = "Variabilité : Réplications")
}

callModule(plotDownloadRate, paste0("Agregats_Paroisses","_Haut"),
           plotFun = reactive(
             Agregats_Paroisses(agregats_data = filtredHaut$agregats, poles_data = filtredHaut$poles) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Agregats_Paroisses","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())
callModule(plotDownloadRate, paste0("Agregats_Paroisses","_Bas"),
           plotFun = reactive(
             Agregats_Paroisses(agregats_data = filtredBas$agregats, poles_data = filtredBas$poles) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Agregats_Paroisses","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())

Agregats_CA <- function(agregats_data){
  
  nombre_agregats <- agregats_data %>%
    filter(communaute == "TRUE") %>%
    group_by(annee, seed) %>%
    summarise(nb = n()) %>%
    collect()
  
  ggplot(nombre_agregats, aes(factor(annee), nb)) +
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre d'agrégats") +
    ggtitle("Évolution du nombre d'agrégats ayant une CA") +
    labs(subtitle = "Variabilité : Réplications")
}

callModule(plotDownloadRate, paste0("Agregats_CA","_Haut"),
           plotFun = reactive(
             Agregats_CA(filtredHaut$agregats) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Agregats_CA","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Agregats_CA","_Bas"),
           plotFun = reactive(
             Agregats_CA(filtredBas$agregats) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Agregats_CA","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())

Agregats_RT <- function(agregats_data){
  
  rtAgregats <- agregats_data %>%
    filter(annee %in% c(820, 960, 1060, 1200)) %>%
    collect() %>%
    group_by(seed, annee) %>%
    mutate(rank = min_rank(-nombre_fp_agregat)) %>%
    group_by(annee, rank) %>%
    summarise(Moyenne = mean(nombre_fp_agregat, na.rm = TRUE),
              Q1 = quantile(nombre_fp_agregat, probs = 0.25),
              Q3 = quantile(nombre_fp_agregat, probs = 0.75)) %>%
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

callModule(plotDownloadRate, paste0("Agregats_RT","_Haut"),
           plotFun = reactive(
             Agregats_RT(filtredHaut$agregats) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Agregats_RT","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Agregats_RT","_Bas"),
           plotFun = reactive(
             Agregats_RT(filtredBas$agregats) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Agregats_RT","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())


output$Agregats_Distribution_Haut <- renderTable({
  req(filtredHaut$agregats)
  nb_fp_breaks <- c(-1,100, 200, 300, 400, 1E12)
  nb_fp_labels <- c("<100", "101-200", "201-300", "301-400", ">400")
  agregats_data <- filtredHaut$agregats
  
  distrib_agregats <- agregats_data %>%
    filter(annee == 1200) %>%
    select(seed, nombre_fp_agregat) %>%
    collect() %>%
    mutate(nb_fp_breaks = cut(nombre_fp_agregat, breaks =nb_fp_breaks, labels = nb_fp_labels)) %>%
    group_by(seed, nb_fp_breaks) %>%
    summarise(nb_agregats = n()) %>%
    group_by(seed) %>%
    mutate(nb_total_agregats = sum(nb_agregats, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(tx_agregats = nb_agregats / nb_total_agregats) %>%
    select(-nb_total_agregats) %>%
    group_by(nb_fp_breaks) %>%
    summarise(NbAgregats_moyen = mean(nb_agregats, na.rm = TRUE),
              TxAgregats_moyen = mean(tx_agregats, na.rm = TRUE)) %>%
    rename(Taille_Agregats = nb_fp_breaks ) %>%
    t() %>%
    as_tibble(rownames = "Type")
  
  colnames(distrib_agregats) <- distrib_agregats[1,]
  distrib_agregats[-1,]
})

output$Agregats_Distribution_Bas <- renderTable({
  req(filtredBas$agregats)
  nb_fp_breaks <- c(-1,100, 200, 300, 400, 1E12)
  nb_fp_labels <- c("<100", "101-200", "201-300", "301-400", ">400")
  agregats_data <- filtredBas$agregats
  
  distrib_agregats <- agregats_data %>%
    filter(annee == 1200) %>%
    select(seed, nombre_fp_agregat) %>%
    collect() %>%
    mutate(nb_fp_breaks = cut(nombre_fp_agregat, breaks =nb_fp_breaks, labels = nb_fp_labels)) %>%
    group_by(seed, nb_fp_breaks) %>%
    summarise(nb_agregats = n()) %>%
    group_by(seed) %>%
    mutate(nb_total_agregats = sum(nb_agregats, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(tx_agregats = nb_agregats / nb_total_agregats) %>%
    select(-nb_total_agregats) %>%
    group_by(nb_fp_breaks) %>%
    summarise(NbAgregats_moyen = mean(nb_agregats, na.rm = TRUE),
              TxAgregats_moyen = mean(tx_agregats, na.rm = TRUE)) %>%
    rename(Taille_Agregats = nb_fp_breaks ) %>%
    t() %>%
    as_tibble(rownames = "Type")
  
  colnames(distrib_agregats) <- distrib_agregats[1,]
  distrib_agregats[-1,]
})