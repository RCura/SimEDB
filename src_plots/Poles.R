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
  req(filtredHaut$poles)
  Poles_Nb(poles_data = filtredHaut$poles)
})

output$Poles_Nb_Filter <- renderPlot({
  req(filtredBas$poles)
  Poles_Nb(poles_data = filtredBas$poles)
})

callModule(plotDownloadRate, paste0("Poles_Nb","_Haut"),
           plotFun = reactive(
             Poles_Nb(filtredHaut$poles) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Poles_Nb","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Poles_Nb","_Bas"),
           plotFun = reactive(
             Poles_Nb(filtredBas$poles) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Poles_Nb","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())


Poles_Agregats <- function(poles_data){
  PolesTous <- poles_data %>%
    group_by(seed, annee) %>%
    summarise(nb_poles = n())
  
  tempVar <- poles_data %>%
    filter(monagregat > 0) %>%
    group_by(seed,annee) %>%
    summarise(nb_pole_ag = n())
  
  PolesAgregats <- PolesTous %>%
    left_join(tempVar, by=c("seed", "annee")) %>%
    mutate(tx_ag = (nb_pole_ag + 1E-12) / (nb_poles + 1E-12)) %>%
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

callModule(plotDownloadRate, paste0("Poles_Agregats","_Haut"),
           plotFun = reactive(
             Poles_Agregats(filtredHaut$poles) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Poles_Agregats","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Poles_Agregats","_Bas"),
           plotFun = reactive(
             Poles_Agregats(filtredBas$poles) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Poles_Agregats","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())



Poles_Compo <- function(poles_data){
  compoPoles <- poles_data %>%
    filter(annee %in% c(820, 960, 1060, 1200)) %>%
    group_by(seed, annee, nb_attracteurs) %>%
    summarise(nb = n()) %>%
    collect()
  
  ggplot(compoPoles, aes(factor(nb_attracteurs), nb)) +
    geom_tufteboxplot() +
    facet_wrap(~annee, scales = "free", nrow = 1) +
    xlab("Nombre d'attracteurs") +
    ylab("Fréquence") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Évolution du nombre d'attracteurs des pôles") +
    labs(subtitle = "Variabilité : Réplications")
}

callModule(plotDownloadRate, paste0("Poles_Compo","_Haut"),
           plotFun = reactive(
             Poles_Compo(filtredHaut$poles) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Poles_Compo","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Poles_Compo","_Bas"),
           plotFun = reactive(
             Poles_Compo(filtredBas$poles) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Poles_Compo","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())


Poles_Attrac <- function(poles_data){
  attracPoles <- poles_data %>%
    filter(annee %in% c(820, 960, 1060, 1200)) %>%
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

callModule(plotDownloadRate, paste0("Poles_Attrac","_Haut"),
           plotFun = reactive(
             Poles_Attrac(filtredHaut$poles) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Poles_Attrac","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Poles_Attrac","_Bas"),
           plotFun = reactive(
             Poles_Attrac(filtredBas$poles) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Poles_Attrac","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())


Poles_RT <- function(poles_data){
  rtPoles_data <- poles_data %>%
    filter(annee %in% c(820, 960, 1060, 1200)) %>%
    collect() %>%
    group_by(seed, annee) %>%
    mutate(rank = min_rank(-nb_attracteurs)) %>%
    group_by(annee, rank) %>%
    summarise(Moyenne = mean(nb_attracteurs),
              Q1 = quantile(nb_attracteurs, probs = 0.25),
              Q3 = quantile(nb_attracteurs, probs = 0.75)) %>%
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
  req(filtredHaut$poles)
  Poles_RT(poles_data = filtredHaut$poles)
})

output$Poles_RT_Filter <- renderPlot({
  req(filtredBas$poles)
  Poles_RT(poles_data = filtredBas$poles)
})

callModule(plotDownloadRate, paste0("Poles_RT","_Haut"),
           plotFun = reactive(
             Poles_RT(filtredHaut$poles) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Poles_RT","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Poles_RT","_Bas"),
           plotFun = reactive(
             Poles_RT(filtredBas$poles) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Poles_RT","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())
