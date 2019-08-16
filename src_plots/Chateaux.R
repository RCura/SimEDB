Chateaux_Nb <- function(chateaux_data){
  ChateauxTous <- chateaux_data %>%
    group_by(seed, annee) %>%
    summarise(nb_chateaux = n()) %>%
    collect()
  
  ggplot(data = ChateauxTous, aes(factor(annee), nb_chateaux)) + 
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre de\nchâteaux") +
    ggtitle("Évolution du nombre de châteaux") +
    labs(subtitle = "Variabilité : Réplications") +
    scale_y_continuous(breaks = seq(0,100, 10), minor_breaks = seq(0,100, 5)) +
    theme_simedb()
}

callModule(plotDownloadRate, paste0("Chateaux_Nb","_Haut"),
           plotFun = reactive(
             Chateaux_Nb(filtredHaut$chateaux) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt))
           ),
           plotName = paste0("Chateaux_Nb","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Chateaux_Nb","_Bas"),
           plotFun = reactive(
             Chateaux_Nb(filtredBas$chateaux) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
           ),
           plotName = paste0("Chateaux_Nb","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())

Chateaux_Proprio <- function(chateaux_data){
  ChateauxProprio <- chateaux_data %>%
    filter(annee == 1200) %>%
    group_by(seed, monproprietaire_type) %>%
    summarise(nb_chateaux = n()) %>%
    collect() %>%
    rename(type_constructeur = monproprietaire_type) %>%
    ungroup()
  
  tousChateaux <- ChateauxProprio %>%
    group_by(seed) %>%
    summarise(nb_chateaux = sum(nb_chateaux), type_constructeur = "Total") %>%
    bind_rows(ChateauxProprio)
  
  cols <- c("Grand Seigneur" = "mediumblue", "Petit Seigneur" = "green4", "Total" = "firebrick2")
  ggplot(data = tousChateaux, aes(factor(type_constructeur), nb_chateaux, fill = type_constructeur, colour= type_constructeur)) + 
    geom_hline(yintercept = 50, linetype = "dashed", colour = "firebrick2", alpha = .75) +
    geom_hline(yintercept = 10, linetype = "dotted", colour = "green4", alpha = .75) +
    geom_tufteboxplot() +
    xlab("Type de constructeur\ndes châteaux") + ylab("Nombre de\nchâteaux") +
    ggtitle("Origine des châteaux") +
    labs(subtitle = "Variabilité : Réplications") +
    scale_colour_manual(values = cols, aesthetics = c("colour", "fill")) +
    theme_simedb() +
    theme(panel.grid.major.x = element_blank()) +
    scale_y_continuous(breaks = seq(0,100, 10), minor_breaks = seq(0,100, 5)) +
    guides(fill = FALSE, colour = FALSE)
}

callModule(plotDownloadRate, paste0("Chateaux_Proprio","_Haut"),
           plotFun = reactive(
             Chateaux_Proprio(filtredHaut$chateaux) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt))
           ),
           plotName = paste0("Chateaux_Proprio","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Chateaux_Proprio","_Bas"),
           plotFun = reactive(
             Chateaux_Proprio(filtredBas$chateaux) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
           ),
           plotName = paste0("Chateaux_Proprio","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())

Chateaux_Type <- function(chateaux_data){
  ChateauxType <- chateaux_data %>%
    filter(annee == 1200) %>%
    group_by(seed, type) %>%
    summarise(nb_chateaux = n()) %>%
    collect() %>%
    ungroup()
  
  tousChateaux <- ChateauxType %>%
    group_by(seed) %>%
    summarise(nb_chateaux = sum(nb_chateaux), type = "Total") %>%
    bind_rows(ChateauxType)
  
  cols <- c("Petit Chateau" = "mediumblue", "Grand Chateau" = "green4", "Total" = "firebrick2")
  ggplot(data = tousChateaux, aes(factor(type), nb_chateaux, fill = type, colour = type)) + 
    geom_hline(yintercept = 50, linetype = "dashed", colour = "firebrick2", alpha = .75) +
    geom_hline(yintercept = 10, linetype = "dotted", colour = "green4", alpha = .75) +
    geom_hline(yintercept = 40, linetype = "dotted", colour = "mediumblue", alpha = .75) +
    geom_tufteboxplot() +
    xlab("Type de châteaux") + ylab("Nombre de\nchâteaux") +
    ggtitle("Types des châteaux") +
    labs(subtitle = "Variabilité : Réplications") +
    scale_colour_manual(values = cols, aesthetics = c("colour", "fill")) +
    theme_simedb()+
    theme(panel.grid.major.x = element_blank()) +
    scale_y_continuous(breaks = seq(0,100, 10), minor_breaks = seq(0,100, 5)) +
    guides(fill = FALSE, colour = FALSE)
  
}

callModule(plotDownloadRate, paste0("Chateaux_Type","_Haut"),
           plotFun = reactive(
             Chateaux_Type(filtredHaut$chateaux) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt))
           ),
           plotName = paste0("Chateaux_Type","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Chateaux_Type","_Bas"),
           plotFun = reactive(
             Chateaux_Type(filtredBas$chateaux) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt))
           ),
           plotName = paste0("Chateaux_Type","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())