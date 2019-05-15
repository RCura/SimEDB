Seigneurs_Nb <- function(seigneurs_data){
  nbSeigneurs <- seigneurs_data %>%
    select(seed, annee, type, nb_chateaux_proprio, nb_chateaux_gardien) %>%
    filter(!(type %in% "Grand Seigneur")) %>%
    group_by(seed, annee, nb_chateaux_proprio, nb_chateaux_gardien, type) %>%
    summarise(n = n()) %>%
    mutate(type = if_else(nb_chateaux_proprio == 0 & nb_chateaux_gardien == 0, "Petit Seigneur", "Chatelain")) %>%
    collect() %>%
    group_by(seed, annee, type) %>%
    summarise(n = sum(n, na.rm = TRUE))
  
  ggplot(nbSeigneurs, aes(factor(annee), col = type, fill = type, y = n)) +
    geom_tufteboxplot() +
    scale_color_discrete(name = "Type de seigneur") +
    scale_fill_discrete(name = "Type de seigneur") +
    ylab("Nombre de seigneurs") +
    xlab("Temps") +
    labs(title = "Évolution du nombre de seigneurs",
         subtitle = "Variabilité : Réplications")
}

callModule(plotDownloadRate, paste0("Seigneurs_Nb","_Haut"),
           plotFun = reactive(
             Seigneurs_Nb(filtredHaut$seigneurs) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Seigneurs_Nb","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Seigneurs_Nb","_Bas"),
           plotFun = reactive(
             Seigneurs_Nb(filtredBas$seigneurs) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Seigneurs_Nb","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())

Seigneurs_Chateaux <- function(seigneurs_data){
  breaksGS <- c(-0,1,2,3,4,5,10,25,50,1000)
  labelsGS <- c("1", "2", "3", "4", "5","6;10", "11;25", "26;50", ">50")
  
  # On ne garde que : les GS, les chatelains, et les PS/Chatelains inits
  
  
  seigneurs_data %>%
    filter(annee == 1200) %>%
    select(seed, sim_name, type, nb_chateaux_proprio, nb_chateaux_gardien) %>%
    filter(nb_chateaux_proprio > 0 | nb_chateaux_gardien > 0) %>%
    mutate(nb_chateaux = nb_chateaux_proprio + nb_chateaux_gardien)
  
  GS <- seigneurs_data %>%
    filter(annee == 1200, type == "Grand Seigneur") %>%
    collect() %>%
    rename(`Propriétés` = nb_chateaux_proprio, Gardiennage = nb_chateaux_gardien) %>%
    gather(key = TypePossession, value = NbChateaux, `Propriétés`, Gardiennage) %>%
    xtabs(formula = ~ seed + type + TypePossession + NbChateaux) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    tbl_df() %>%
    mutate(NbChateaux = as.numeric(NbChateaux)) %>%
    filter(NbChateaux > 0) %>%
    mutate(NbChateauxBreaks =  cut(NbChateaux, breaks = breaksGS, labels =  labelsGS)) %>%
    group_by(seed, NbChateauxBreaks, TypePossession, type) %>%
    summarise(NbSeigneurs = sum(Freq))
  
  breaksChat <- c(-1,0,1,2,3,4,1000)
  labelsChat <- c("0", "1", "2", "3", "4", "5+")
  
  Chat <- seigneurs_data %>%
    filter(annee == 1200, type == "Petit Seigneur") %>%
    collect() %>%
    rename(`Propriétés` = nb_chateaux_proprio, Gardiennage = nb_chateaux_gardien) %>%
    gather(key = TypePossession, value = NbChateaux, `Propriétés`, Gardiennage) %>%
    xtabs(formula = ~ seed + type + TypePossession + NbChateaux) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    tbl_df() %>%
    mutate(NbChateaux = as.numeric(NbChateaux)) %>%
    filter(NbChateaux > 0) %>%
    mutate(NbChateauxBreaks =  cut(NbChateaux, breaks = breaksChat, labels =  labelsChat)) %>%
    group_by(seed, NbChateauxBreaks, TypePossession, type) %>%
    summarise(NbSeigneurs = sum(Freq))
  
  plotGS <- ggplot(data = GS, aes(NbChateauxBreaks, NbSeigneurs)) +
    geom_tufteboxplot() +
    facet_wrap(~TypePossession, scales = "free") +
    ggtitle("Grands Seigneurs") + 
    theme(axis.title.y = element_blank(), axis.title.x = element_blank()) +
    theme(plot.title = element_text(size = rel(1), face = "italic")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  plotChat <- ggplot(data = Chat, aes(NbChateauxBreaks, NbSeigneurs)) +
    geom_tufteboxplot() + 
    facet_wrap(~TypePossession, scales = "free") +
    ggtitle("Chatelains") + 
    theme(axis.title.y = element_blank(),axis.title.x = element_blank()) +
    theme(plot.title = element_text(size = rel(1), face = "italic")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  grid.arrange(plotChat, plotGS, nrow = 2,
               bottom = "Nombre de châteaux", left = "Fréquence",
               top = "Distribution des possessions et gardiennages de châteaux
               Variabilité : Réplications"
  )
}

callModule(plotDownloadRate, paste0("Seigneurs_Chateaux","_Haut"),
           plotFun = reactive(
             Seigneurs_Chateaux(filtredHaut$seigneurs) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Seigneurs_Chateaux","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Seigneurs_Chateaux","_Bas"),
           plotFun = reactive(
             Seigneurs_Chateaux(filtredBas$seigneurs) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Seigneurs_Chateaux","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())

Seigneurs_Vassaux <- function(seigneurs_data){
  myBreaks <- c(-1,0,1,2,3,4,5,10,25,50,1000)
  myLabels <- c("0","1", "2", "3", "4", "5","6;10", "11;25", "26;50", ">50")
  
  debiteurs_seigneurs <- seigneurs_data %>%
    filter(annee == 1200) %>%
    select(seed, annee, type, seigneur_initial, nbdebiteurs) %>%
    collect() %>%
    mutate(initial = if_else(seigneur_initial == "TRUE", "Initialement\nPrésent", "Arrivé\nen cours")) %>%
    mutate(initial = factor(initial, levels = c("Arrivé\nen cours", "Initialement\nPrésent"))) %>%
    mutate(nbDebiteursBreaks =  cut(nbdebiteurs, breaks = myBreaks, labels =  myLabels)) %>%  
    group_by(seed, type, initial, nbDebiteursBreaks) %>%
    summarise(StatsDebiteurs = n())
  
  debInitCPS <- debiteurs_seigneurs %>% filter(type != "Grand Seigneur", initial == "Initialement\nPrésent")
  debNonCPS <- debiteurs_seigneurs %>% filter(type != "Grand Seigneur", initial == "Arrivé\nen cours")
  debGS <- debiteurs_seigneurs %>% filter(type == "Grand Seigneur")
  
  plotInitCPS <- ggplot(debInitCPS, aes(nbDebiteursBreaks, StatsDebiteurs)) +
    geom_tufteboxplot() +
    facet_grid(initial ~ type, scales="free", space = "free_x") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
    theme(plot.title = element_text(size = rel(1), face = "italic"))
  
  plotNonCPS <- ggplot(debNonCPS, aes(nbDebiteursBreaks, StatsDebiteurs)) +
    geom_tufteboxplot() +
    facet_grid(initial ~ type, scales="free", space = "free_x") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
    theme(plot.title = element_text(size = rel(1), face = "italic"))
  
  plotGS <- ggplot(debGS, aes(nbDebiteursBreaks, StatsDebiteurs)) +
    geom_tufteboxplot() +
    facet_wrap(~type) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
    theme(plot.title = element_text(size = rel(1), face = "italic"))
  
  lay <- rbind(c(1,1,1,3),
               c(2,2,2,3))
  
  grid.arrange(plotNonCPS, plotInitCPS, plotGS, nrow = 1, layout_matrix = lay,
               bottom = "Nombre de Vassaux", left = "Fréquence",
               top = "Distribution du nombre de vassaux selon les types de seigneurs
               Variabilité : Réplications")
}

callModule(plotDownloadRate, paste0("Seigneurs_Vassaux","_Haut"),
           plotFun = reactive(
             Seigneurs_Vassaux(filtredHaut$seigneurs) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Seigneurs_Vassaux","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Seigneurs_Vassaux","_Bas"),
           plotFun = reactive(
             Seigneurs_Vassaux(filtredBas$seigneurs) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Seigneurs_Vassaux","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())


# Seigneurs_Redevances <- function(seigneurs_data){
#   redevances_seigneurs <- seigneurs_data %>%
#     filter(annee == 1200) %>%
#     select(seed, annee, type, nb_fp_assujettis) %>%
#     collect() %>%
#     mutate(type = factor(type, levels = c("Petit Seigneur", "Chatelain", "Grand Seigneur")))
#   
#   ggplot(redevances_seigneurs, aes(type, nb_fp_assujettis)) +
#     geom_tufteboxplot() +
#     scale_y_log10(breaks = c(10,50,100, 500,1000, 2000)) +
#     xlab("Types de seigneurs") + ylab("Nombre de FP assujetis\n(Échelle logarithmique)") +
#     ggtitle("Distribution des redevances en fin de simulation") +
#     labs(subtitle = "Variabilité : Seigneurs et réplications")
# }
# 
# callModule(plotDownloadRate, paste0("Seigneurs_Redevances","_Haut"),
#            plotFun = reactive(
#              Seigneurs_Redevances(filtredHaut$seigneurs) +
#                labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
#                theme(plot.caption = element_text(size = 6, hjust = 0))
#            ),
#            plotName = paste0("Seigneurs_Redevances","_Haut"),
#            user = input$userName,
#            seeds = filtredSeedsHaut_plotly())
# 
# callModule(plotDownloadRate, paste0("Seigneurs_Redevances","_Bas"),
#            plotFun = reactive(
#              Seigneurs_Redevances(filtredBas$seigneurs) +
#                labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
#                theme(plot.caption = element_text(size = 6, hjust = 0))
#            ),
#            plotName = paste0("Seigneurs_Redevances","_Bas"),
#            user = input$userName,
#            seeds = filtredSeedsBas_plotly())

# Seigneurs_Redevances_PS <- function(seigneurs_data){
#   x <- "nb_fp_assujettis"
#   redevancesLevels <- c("0","1-5","6-15","16-30","30-100",">100")
#   redevancesBreaks <- rlang::exprs(
#     .data[[x]] == 0 ~ "0",
#     .data[[x]] <= 5 ~ "1-5",
#     .data[[x]] <= 15 ~ "6-15",
#     .data[[x]] <= 30 ~ "16-30",
#     .data[[x]] <= 100 ~ "30-100",
#     .data[[x]] > 100 ~ ">100"
#   )
#   
#   redevances_PS <- seigneurs_data %>%
#     filter(annee == 1200) %>%
#     filter(!(type %in% "Grand Seigneur")) %>%
#     select(seed, annee, type, nb_fp_assujettis) %>%
#     collect() %>%
#     mutate(type = factor(type, levels = c("Petit Seigneur", "Chatelain"))) %>%
#     mutate(nbFP_cut = case_when(!!!redevancesBreaks)) %>%
#     mutate(nbFP_cut =  factor(nbFP_cut, levels = redevancesLevels)) %>%
#     group_by(seed, type,nbFP_cut) %>%
#     summarise(N = n())
#   
#   ggplot(redevances_PS, aes(nbFP_cut, N)) +
#     geom_tufteboxplot() +
#     facet_wrap(~type) +
#     xlab("Nombre de FP assujetis\n(Échelle logarithmique)") +
#     ylab("Nombre de seigneurs") +
#     ggtitle("Distribution des redevances en fin de simulation") +
#     labs(subtitle = "Variabilité : Réplications") +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
# }
# 
# callModule(plotDownloadRate, paste0("Seigneurs_Redevances_PS","_Haut"),
#            plotFun = reactive(
#              Seigneurs_Redevances_PS(filtredHaut$seigneurs) +
#                labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
#                theme(plot.caption = element_text(size = 6, hjust = 0))
#            ),
#            plotName = paste0("Seigneurs_Redevances_PS","_Haut"),
#            user = input$userName,
#            seeds = filtredSeedsHaut_plotly())
# 
# callModule(plotDownloadRate, paste0("Seigneurs_Redevances_PS","_Bas"),
#            plotFun = reactive(
#              Seigneurs_Redevances_PS(filtredBas$seigneurs) +
#                labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
#                theme(plot.caption = element_text(size = 6, hjust = 0))
#            ),
#            plotName = paste0("Seigneurs_Redevances_PS","_Bas"),
#            user = input$userName,
#            seeds = filtredSeedsBas_plotly())

Seigneurs_Redevances_PS <- function(seigneurs_data){ # Nouvelle version pour v6.3
  global_data <- seigneurs_data %>%
    filter(annee == 1200) %>%
    select(seed, sim_name, type, nb_chateaux_proprio:nb_chateaux_gardien,nb_fp_assujettis:nb_fp_autres_droits_garde) %>%
    collect() %>%
    mutate(type = case_when(
      type == "Grand Seigneur" ~ "Grand Seigneur",
      nb_chateaux_proprio > 0 ~ "Châtelain",
      nb_chateaux_gardien > 0 ~ "Châtelain",
      TRUE ~ type
    )) %>%
    select(-nb_chateaux_proprio, -nb_chateaux_gardien) %>%
    gather(Indicateur, NbFP, -seed, -sim_name, -type) %>%
    mutate(Indicateur = str_remove_all(Indicateur, "nb_fp_")) %>%
    mutate(type = factor(type, levels = c("Petit Seigneur", "Châtelain", "Grand Seigneur")))
  
  assujettis <- global_data %>%
    filter(Indicateur == "assujettis") %>%
    mutate(Indicateur = "FP assujettis") %>%
    mutate(type_prelevement = "Direct")
  
  plot_data <- global_data %>%
    filter(Indicateur != "assujettis") %>%
    mutate(type_prelevement = if_else(str_detect(Indicateur, "_garde"),
                                      true = "Garde",
                                      false = 'Direct')) %>%
    mutate(Indicateur = str_remove(Indicateur, "_garde")) %>%
    mutate(Indicateur = case_when(
      Indicateur == "autres_droits" ~ "Autres droits",
      Indicateur == "foncier" ~ "Droits Fonciers",
      Indicateur == "haute_justice" ~ "Haute Justice",
      TRUE ~ Indicateur
    )) %>%
    bind_rows(assujettis) %>%
    mutate(Indicateur = factor(Indicateur, levels = c("Autres droits", "Droits Fonciers", "Haute Justice", "FP assujettis"))) %>%
    filter(NbFP > 0)
    mutate(NbFP = NbFP+.5)

  library(grid)
  ggplot(plot_data) +
    aes(NbFP, fill = type_prelevement) +
    geom_histogram(alpha = .5, bins = 20) +
    facet_wrap(type~Indicateur, scales = "free", labeller = labeller(.multi_line = FALSE), ncol = 4, nrow = 3) +
    scale_x_log10() +
    scale_y_log10() +
    xlab("Nombre de foyers paysans assujettis par seigneur en fin de simulation") +
    ylab("Fréquence") +
    ggtitle("Distribution des droits prélevés par les seigneurs") +
    scale_fill_discrete(name = "Type de prélèvement") +
    theme(strip.text = element_text(size = 7),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          #legend.position = "bottom"
          panel.spacing.y = unit(0, "lines"),
    ) +
    labs(subtitle = "Variabilité : Réplications")
  
}

callModule(plotDownloadRate, paste0("Seigneurs_Redevances_PS","_Haut"),
           plotFun = reactive(
             Seigneurs_Redevances_PS(filtredHaut$seigneurs) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Seigneurs_Redevances_PS","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Seigneurs_Redevances_PS","_Bas"),
           plotFun = reactive(
             Seigneurs_Redevances_PS(filtredBas$seigneurs) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Seigneurs_Redevances_PS","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())

Seigneurs_Puissance <- function(seigneurs_data){
  seigneurs_puissance <- seigneurs_data %>%
    filter(puissance > 0) %>%
    group_by(seed, annee, type) %>%
    collect() %>%
    summarise(Q1 = quantile(puissance, 0.25),
              Mean = mean(puissance),
              Max = max(puissance)) %>%
    gather(TypeIndic, puissance, Q1:Max)
  
  ggplot(seigneurs_puissance, aes(factor(annee), puissance)) +
    geom_tufteboxplot() +
    facet_grid(type~TypeIndic, scales = "free") +
    xlab("Temps") + ylab("Puissance") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Évolution de puissance des seigneurs\n(Puissance > 0, ≈50% des seigneurs)") +
    labs(subtitle = "Variabilité : Réplications")
}

callModule(plotDownloadRate, paste0("Seigneurs_Puissance","_Haut"),
           plotFun = reactive(
             Seigneurs_Puissance(filtredHaut$seigneurs) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Seigneurs_Puissance","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Seigneurs_Puissance","_Bas"),
           plotFun = reactive(
             Seigneurs_Puissance(filtredBas$seigneurs) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Seigneurs_Puissance","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())



Seigneurs_Agregats <- function(seigneurs_data, agregats_data){
  x <- "nbAgregats"
  nbAgregatsLevels <- c("0","1","2","3-5",">5")
  nbAgregatsBreaks <- rlang::exprs(
    .data[[x]] == 0 ~ "0",
    .data[[x]] == 1 ~ "1",
    .data[[x]] == 2 ~ "2",
    .data[[x]] <= 5 ~ "3-5",
    .data[[x]] > 5 ~ ">5"
  )
  
  seigneurs_agregats <- seigneurs_data %>%
    filter(annee ==  1200) %>%
    group_by(seed, monagregat) %>%
    summarise(nbAgregats = n()) %>%
    collect() %>%
    mutate(NbAgregats = case_when(!!!nbAgregatsBreaks)) %>%
    mutate(NbAgregats = factor(NbAgregats, levels = nbAgregatsLevels)) %>%
    right_join(agregats_data %>%
                 filter(annee == 1200) %>%
                 select(seed, id_agregat) %>%
                 collect(),
               by = c("seed", "monagregat" = "id_agregat")) %>%
    mutate(NbAgregats = fct_explicit_na(NbAgregats, "0")) %>%
    group_by(seed, NbAgregats) %>%
    summarise(NbCas = n())
  
  ggplot(seigneurs_agregats, aes(factor(NbAgregats), NbCas)) +
    geom_tufteboxplot() +
    labs(x = "Nombre de seigneurs par agrégat",
         y = "Nombre d'agrégats",
         title = "Nombre de seigneurs par agrégat en fin de simulation",
         subtitle = "Variabilité : Réplications",
         caption = "N.B : Sont considérés comme dans un agrégat les seigneurs localisés à moins de 200m")
}

callModule(plotDownloadRate, paste0("Seigneurs_Agregats","_Haut"),
           plotFun = reactive(
             Seigneurs_Agregats(seigneurs_data = filtredHaut$seigneurs,
                                agregats_data = filtredHaut$agregats) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Seigneurs_Agregats","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())

callModule(plotDownloadRate, paste0("Seigneurs_Agregats","_Bas"),
           plotFun = reactive(
             Seigneurs_Agregats(seigneurs_data = filtredHaut$seigneurs,
                                agregats_data = filtredHaut$agregats) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("Seigneurs_Agregats","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())
