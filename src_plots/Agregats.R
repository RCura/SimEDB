Agregats_Nb <- function(agregats_data){
  nombre_agregats <- agregats_data %>%
    group_by(annee, seed) %>%
    summarise(nb = n()) %>%
    collect()
  
  ggplot(nombre_agregats, aes(factor(annee), nb)) +
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre d'agrégats") +
    ggtitle("Évolution du nombre d'agrégats de population") +
    labs(subtitle = "Variabilité : Réplications") +
    theme_simedb()
}

callModule(plotDownloadRate, paste0("Agregats_Nb","_Haut"),
           plotFun = reactive(
             Agregats_Nb(filtredHaut$agregats) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt))
           ),
           plotName = paste0("Agregats_Nb","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)
callModule(plotDownloadRate, paste0("Agregats_Nb","_Bas"),
           plotFun = reactive(
             Agregats_Nb(filtredBas$agregats) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
           ),
           plotName = paste0("Agregats_Nb","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)

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
    labs(subtitle = "Variabilité : Réplications") +
    theme_simedb()
}

callModule(plotDownloadRate, paste0("Agregats_Poles","_Haut"),
           plotFun = reactive(
             Agregats_Poles(filtredHaut$agregats) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt))
           ),
           plotName = paste0("Agregats_Poles","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)

callModule(plotDownloadRate, paste0("Agregats_Poles","_Bas"),
           plotFun = reactive(
             Agregats_Poles(filtredBas$agregats) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
           ),
           plotName = paste0("Agregats_Poles","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)

Agregats_Paroisses <- function(agregats_data, poles_data){
  nb_agregats <- agregats_data %>%
    group_by(seed, sim_name, annee) %>%
    summarise(nb_agregats = n()) %>%
    ungroup()%>%
    collect()
  
  nb_agregats_paroisses <- agregats_data %>%
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
    mutate(taux_agregats = nb_agregats_paroisses / nb_agregats)
  
  ggplot(plot_data) +
    aes(factor(annee), taux_agregats) +
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Part des agrégats\ncontenant au moins une paroisse") +
    ggtitle("Évolution du taux d'agrégats contenant au moins une paroisse") +
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    labs(subtitle = "Variabilité : Réplications") +
    theme_simedb()
}

callModule(plotDownloadRate, paste0("Agregats_Paroisses","_Haut"),
           plotFun = reactive(
             Agregats_Paroisses(agregats_data = filtredHaut$agregats, poles_data = filtredHaut$poles) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt))
           ),
           plotName = paste0("Agregats_Paroisses","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)

callModule(plotDownloadRate, paste0("Agregats_Paroisses","_Bas"),
           plotFun = reactive(
             Agregats_Paroisses(agregats_data = filtredBas$agregats, poles_data = filtredBas$poles) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
           ),
           plotName = paste0("Agregats_Paroisses","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)


Agregats_NbParoisses <- function(poles_data){
  Agregats_NbParoisses_data <- poles_data %>%
    filter(annee %in% c(820, 900, 1000, 1100, 1200)) %>%
    filter(monagregat > -1) %>%
    filter(nb_ca < 2) %>% # On enlève les pôles contenant plusieur CA (il y en a 2k / 285k par exemple)
    select(seed, sim_name, annee, nb_paroisses) %>%
    group_by(seed, sim_name, annee, nb_paroisses) %>%
    summarise(nb = n()) %>%
    collect() %>%
    arrange(nb_paroisses) %>%
    mutate(nb_paroisses_breaks = case_when(
      nb_paroisses > 10 ~ as.character('>10'),
      nb_paroisses >= 6 ~ as.character('6-10'),
      TRUE ~ as.character(nb_paroisses)
    )) %>%
    mutate(nb_paroisses_breaks = factor(nb_paroisses_breaks,
                                        levels = c("0", "1", "2", "3", "4", "5","6-10", ">10"))) %>%
    group_by(seed, sim_name, annee, nb_paroisses_breaks) %>%
    summarise(nbPolesAg = sum(nb, na.rm = TRUE))
  
  
    ggplot(Agregats_NbParoisses_data) +
    aes(nb_paroisses_breaks, nbPolesAg) +
    geom_tufteboxplot() +
    facet_wrap(~annee, nrow = 1) +
    xlab("Nombre de paroisses par agrégat") +
    ylab("Fréquence") +
    ggtitle("Distribution des paroisses par agrégat\n(agrégats membre d'un pôle uniquement)")  +
    labs(subtitle = "Variabilité : Réplications") +
    theme_simedb_rotate_x()
}

callModule(plotDownloadRate, paste0("Agregats_NbParoisses","_Haut"),
           plotFun = reactive(
             Agregats_NbParoisses(poles_data = filtredHaut$poles) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt))
           ),
           plotName = paste0("Agregats_NbParoisses","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)

callModule(plotDownloadRate, paste0("Agregats_NbParoisses","_Bas"),
           plotFun = reactive(
             Agregats_NbParoisses(poles_data = filtredBas$poles) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
           ),
           plotName = paste0("Agregats_NbParoisses","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)

Agregats_CA <- function(agregats_data){
  
  nombre_agregats <- agregats_data %>%
    group_by(seed, sim_name, annee) %>%
    summarise(nb_agregats = n())
    
    taux_agregats <- agregats_data %>%
      filter(communaute == "TRUE") %>%
      group_by(seed, sim_name, annee) %>%
      summarise(nb_coms = n()) %>%
      left_join(nombre_agregats, by = c("sim_name", "seed", "annee")) %>%
      mutate(taux = (nb_coms+1E-9) / (nb_agregats+1E-9)) %>%
      collect()
  
  ggplot(taux_agregats, aes(factor(annee), taux)) +
    geom_tufteboxplot() +
    xlab("Temps") + ylab("% des agrégats") +
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    ggtitle("Évolution de la part des agrégats ayant une communauté villageoise") +
    labs(subtitle = "Variabilité : Réplications") +
    theme_simedb()
}

callModule(plotDownloadRate, paste0("Agregats_CA","_Haut"),
           plotFun = reactive(
             Agregats_CA(filtredHaut$agregats) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt))
           ),
           plotName = paste0("Agregats_CA","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)

callModule(plotDownloadRate, paste0("Agregats_CA","_Bas"),
           plotFun = reactive(
             Agregats_CA(filtredBas$agregats) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
           ),
           plotName = paste0("Agregats_CA","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)

Agregats_RT <- function(agregats_data){
  
  rtAgregats <- agregats_data %>%
    filter(annee %in% c(820, 900, 1000, 1100, 1200)) %>%
    collect() %>%
    group_by(seed, annee) %>%
    mutate(rank = row_number(-nombre_fp_agregat)) %>%
    group_by(annee, rank) %>%
    summarise(Moyenne = mean(nombre_fp_agregat, na.rm = TRUE),
              Q1 = quantile(nombre_fp_agregat, probs = 0.25),
              Q3 = quantile(nombre_fp_agregat, probs = 0.75)) %>%
    gather(key = `Méthode d'agrégation`, value = Value, Moyenne:Q3) %>%
    ungroup()
  
  ggplot(rtAgregats, aes(rank, Value, group = `Méthode d'agrégation`, colour = `Méthode d'agrégation`)) +
    geom_line(size = 0.3, linetype = "dotted") +
    geom_point(size = 0.3) +
    scale_color_manual(values = c("black", "red", "blue"), name = NULL) +
    facet_grid(~annee, space = "free_x",  scales = "free_x") +
    scale_x_log10() + scale_y_log10() +
    ggtitle("Évolution de la distribution rang-taille des agrégats de population") +
    xlab("Rang (log10)") + ylab("Taille des agrégats\n(nombre de foyers paysans)") +
    labs(subtitle = "Variabilité : Moyenne, Q1 et Q3 des Réplications") +
    theme_simedb()
}

callModule(plotDownloadRate, paste0("Agregats_RT","_Haut"),
           plotFun = reactive(
             Agregats_RT(filtredHaut$agregats) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt))
           ),
           plotName = paste0("Agregats_RT","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)

callModule(plotDownloadRate, paste0("Agregats_RT","_Bas"),
           plotFun = reactive(
             Agregats_RT(filtredBas$agregats) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
           ),
           plotName = paste0("Agregats_RT","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)


agregats_distribution <- function(agregats_data){
  nb_fp_breaks <- c(-1,100, 200, 300, 400, 600, 1E12)
  nb_fp_labels <- c("<100", "101-200", "201-300", "301-400", "401-600",">600")
  
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
    summarise(`Nombre moyen` = mean(nb_agregats, na.rm = TRUE),
              `Taux moyen` = mean(tx_agregats, na.rm = TRUE)) %>%
    rename(`Nombre de foyers paysans` = nb_fp_breaks ) %>%
    t() %>%
    as_tibble(rownames = "Type") %>%
    set_colnames(.[1,]) %>%
    slice(-1)
  
  distrib_agregats[1,2:6] <- as.character(round(as.numeric(distrib_agregats[1,2:6]), digits = 2))
  distrib_agregats[2,2:6] <- paste0(as.character(round(as.numeric(distrib_agregats[2,2:6]) * 100, digits = 1)), "%")
  distrib_agregats
}

output$Agregats_Distribution_Haut <- renderTable({
  req(filtredHaut$agregats)
  agregats_distribution(agregats_data = filtredHaut$agregats)
})

output$Agregats_Distribution_Bas <- renderTable({
  req(filtredBas$agregats)
  agregats_distribution(agregats_data = filtredBas$agregats)
})



agregats_taille <- function(agregats_data){
  agregats_data %>%
    select(seed, annee, nombre_fp_agregat) %>%
    filter(annee == 1200) %>%
    arrange(desc(nombre_fp_agregat)) %>%
    collect() %>%
    group_by(seed) %>%
    mutate(rank = row_number(-nombre_fp_agregat)) %>%
    ungroup() %>%
    filter(rank <= 4) %>%
    group_by(rank) %>%
    summarise(mean = mean(nombre_fp_agregat, na.rm = TRUE),
              med = median(nombre_fp_agregat, na.rm = TRUE),
              sd = sd(nombre_fp_agregat, na.rm = TRUE),
              min = min(nombre_fp_agregat, na.rm = TRUE),
              max = max(nombre_fp_agregat, na.rm = TRUE)
              ) %>%
    transmute(
      `Rang de l'agrégat` = rank,
      Moyenne = as.integer(round(mean)),
      `Médiane` = as.integer(round(med)),
      StDev = as.integer(round(sd)),
      Min = as.integer(round(min)),
      Max = as.integer(round(max)))
}

output$Agregats_Taille_Haut <- renderTable({
  req(filtredHaut$agregats)
  agregats_taille(agregats_data = filtredHaut$agregats)
})

output$Agregats_Taille_Bas <- renderTable({
  req(filtredBas$agregats)
  agregats_taille(agregats_data = filtredBas$agregats)
})


Agregats_Carte <- function(agregats_data){
  
  random_seeds <- agregats_data %>%
    select(seed) %>%
    group_by(seed) %>%
    tally() %>%
    collect() %>%
    sample_n(size = 2) %>%
    pull(seed)
  
  random_seeds <- agregats_data %>%
    select(seed) %>%
    group_by(seed) %>%
    tally() %>%
    head(2) %>%
    pull(seed)
  
  agregats_choisis <- agregats_data %>%
    filter(annee %in% c(820, 900, 1000, 1100, 1200)) %>%
    filter(seed %in% local(random_seeds)) %>%
    select(seed, annee, nombre_fp_agregat, communaute, geom) %>%
    filter(nombre_fp_agregat >= 20) %>%
    collect() %>%
    st_as_sf(wkt = "geom") %>%
    st_centroid(., ) %>%
    cbind(st_coordinates(.)) %>%
    st_drop_geometry() %>%
    as_tibble() %>%
    rename(`Année` = annee) %>%
    arrange(seed) %>%
    group_by(seed) %>%
    mutate(Simulation = group_indices()) %>%
    ungroup()
  
  ggplot(agregats_choisis) +
    aes(X, Y, size = nombre_fp_agregat, colour = communaute) +
    geom_point() +
    scale_color_manual(values = c("black", "red"), name = "Communauté rurale ?") +
    scale_size_continuous(name = "Nombre de Foyers Paysans", range = c(.01, 3),
                          breaks = c(10,50,100, 250, 500, 1000, 2000)) +
    coord_fixed() +
    facet_grid(Simulation~`Année`, labeller = label_both) +
    ggtitle("Population des agrégats au cours du temps") +
    xlab("") + ylab("") +
    labs(subtitle = "Variabilité : Aucune / Agrégats représentés : population >= 20") +
    theme_simedb_map()
}

callModule(plotDownloadRate, paste0("Agregats_Carte","_Haut"),
           plotFun = reactive(
             Agregats_Carte(filtredHaut$agregats) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt))
           ),
           plotName = paste0("Agregats_Carte","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)

callModule(plotDownloadRate, paste0("Agregats_Carte","_Bas"),
           plotFun = reactive(
             Agregats_Carte(filtredBas$agregats) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
           ),
           plotName = paste0("Agregats_Carte","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly(),
           plotwidth = plotWidth,
           plotheight = plotHeight)
