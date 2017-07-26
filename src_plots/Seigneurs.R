Seigneurs_Nb <- function(seigneurs_data){
  nbSeigneurs <- seigneurs_data %>%
    filter(type != "Grand Seigneur") %>%
    group_by(seed, annee, type) %>%
    summarise(n = n()) %>%
    collect()
  
  ggplot(nbSeigneurs, aes(factor(annee), col = type, fill = type, y = n)) +
    geom_tufteboxplot() +
    scale_color_discrete(name = "Type de seigneur") +
    scale_fill_discrete(name = "Type de seigneur") +
    ylab("Nombre de seigneurs") +
    xlab("Temps") +
    labs(title = "Évolution du nombre de seigneurs",
         subtitle = "Variabilité : Réplications")
}

output$Seigneurs_Nb <- renderPlot({
  req(filtredHaut$seigneurs)
  Seigneurs_Nb(seigneurs_data = filtredHaut$seigneurs)
})

output$Seigneurs_Nb_Filter <- renderPlot({
  req(filtredBas$seigneurs)
  Seigneurs_Nb(seigneurs_data = filtredBas$seigneurs)
})

Seigneurs_Chateaux <- function(seigneurs_data){
  breaksGS <- c(-0,1,2,3,4,5,10,25,50,1000)
  labelsGS <- c("1", "2", "3", "4", "5","6;10", "11;25", "26;50", ">50")
  
  # On ne garde que : les GS, les chatelains, et les PS/Chatelains inits
  
  GS <- seigneurs_data %>%
    filter(annee == 1160, type == "Grand Seigneur") %>%
    collect() %>%
    rename(`Propriétés` = nbchateauxproprio, Gardiennage = nbchateauxgardien) %>%
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
    filter(annee == 1160, type == "Chatelain") %>%
    collect() %>%
    rename(`Propriétés` = nbchateauxproprio, Gardiennage = nbchateauxgardien) %>%
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

output$Seigneurs_Chateaux <- renderPlot({
  req(filtredHaut$seigneurs)
  Seigneurs_Chateaux(seigneurs_data = filtredHaut$seigneurs)
})

output$Seigneurs_Chateaux_Filter <- renderPlot({
  req(filtredBas$seigneurs)
  Seigneurs_Chateaux(seigneurs_data = filtredBas$seigneurs)
})

Seigneurs_Vassaux <- function(seigneurs_data){
  myBreaks <- c(-1,0,1,2,3,4,5,10,25,50,1000)
  myLabels <- c("0","1", "2", "3", "4", "5","6;10", "11;25", "26;50", ">50")
  
  debiteurs_seigneurs <- seigneurs_data %>%
    filter(annee == 1160) %>%
    select(seed, annee, type, initial, nbdebiteurs) %>%
    collect() %>%
    mutate(initial = if_else(initial == 1, "Initialement\nPrésent", "Arrivé\nen cours")) %>%
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

output$Seigneurs_Vassaux <- renderPlot({
  req(filtredHaut$seigneurs)
  Seigneurs_Vassaux(seigneurs_data = filtredHaut$seigneurs)
})

output$Seigneurs_Vassaux_Filter <- renderPlot({
  req(filtredBas$seigneurs)
  Seigneurs_Vassaux(seigneurs_data = filtredBas$seigneurs)
})

Seigneurs_Redevances <- function(seigneurs_data){
  redevances_seigneurs <- seigneurs_data %>%
    filter(annee == 1160) %>%
    select(seed, annee, type, nbfpassujettis) %>%
    collect() %>%
    mutate(type = factor(type, levels = c("Petit Seigneur", "Chatelain", "Grand Seigneur")))
  
  ggplot(redevances_seigneurs, aes(type, nbfpassujettis)) +
    geom_tufteboxplot() +
    scale_y_log10(breaks = c(10,50,100, 500,1000, 2000)) +
    xlab("Types de seigneurs") + ylab("Nombre de FP assujetis\n(Échelle logarithmique)") +
    ggtitle("Distribution des redevances en fin de simulation") +
    labs(subtitle = "Variabilité : Seigneurs et réplications")
}

output$Seigneurs_Redevances <- renderPlot({
  req(filtredHaut$seigneurs)
  Seigneurs_Redevances(seigneurs_data = filtredHaut$seigneurs)
})

output$Seigneurs_Redevances_Filter <- renderPlot({
  req(filtredBas$seigneurs)
  Seigneurs_Redevances(seigneurs_data = filtredBas$seigneurs)
})

Seigneurs_Redevances_PS <- function(seigneurs_data){
  x <- "nbfpassujettis"
  redevancesLevels <- c("0","1-5","6-15","16-30","30-100",">100")
  redevancesBreaks <- rlang::exprs(
    .data[[x]] == 0 ~ "0",
    .data[[x]] <= 5 ~ "1-5",
    .data[[x]] <= 15 ~ "6-15",
    .data[[x]] <= 30 ~ "16-30",
    .data[[x]] <= 100 ~ "30-100",
    .data[[x]] > 100 ~ ">100"
  )
  
  redevances_PS <- seigneurs_data %>%
    filter(annee == 1160) %>%
    filter(type != "Grand Seigneur") %>%
    select(seed, annee, type, nbfpassujettis) %>%
    collect() %>%
    mutate(type = factor(type, levels = c("Petit Seigneur", "Chatelain"))) %>%
    mutate(nbFP_cut = case_when(!!!redevancesBreaks)) %>%
    mutate(nbFP_cut =  factor(nbFP_cut, levels = redevancesLevels)) %>%
    group_by(seed, type,nbFP_cut) %>%
    summarise(N = n())
  
  ggplot(redevances_PS, aes(nbFP_cut, N)) +
    geom_tufteboxplot() +
    facet_wrap(~type) +
    xlab("Nombre de FP assujetis\n(Échelle logarithmique)") +
    ylab("Nombre de seigneurs") +
    ggtitle("Distribution des redevances en fin de simulation") +
    labs(subtitle = "Variabilité : Réplications") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

output$Seigneurs_Redevances_PS <- renderPlot({
  req(filtredHaut$seigneurs)
  Seigneurs_Redevances_PS(seigneurs_data = filtredHaut$seigneurs)
})

output$Seigneurs_Redevances_PS_Filter <- renderPlot({
  req(filtredBas$seigneurs)
  Seigneurs_Redevances_PS(seigneurs_data = filtredBas$seigneurs)
})

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

output$Seigneurs_Puissance <- renderPlot({
  req(filtredHaut$seigneurs)
  Seigneurs_Puissance(seigneurs_data = filtredHaut$seigneurs)
})

output$Seigneurs_Puissance_Filter <- renderPlot({
  req(filtredBas$seigneurs)
  Seigneurs_Puissance(seigneurs_data = filtredBas$seigneurs)
})


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
    filter(annee ==  1160) %>%
    group_by(seed, monagregat) %>%
    summarise(nbAgregats = n()) %>%
    collect() %>%
    mutate(NbAgregats = case_when(!!!nbAgregatsBreaks)) %>%
    mutate(NbAgregats = factor(NbAgregats, levels = nbAgregatsLevels)) %>%
    right_join(agregats_data %>%
                 filter(annee == 1160) %>%
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

output$Seigneurs_Agregats <- renderPlot({
  req(filtredHaut$seigneurs, filtredHaut$agregats)
  Seigneurs_Agregats(seigneurs_data = filtredHaut$seigneurs,
                     agregats_data = filtredHaut$agregats)
})

output$Seigneurs_Agregats_Filter <- renderPlot({
  req(filtredBas$seigneurs, filtredBas$agregats)
  Seigneurs_Agregats(seigneurs_data = filtredBas$seigneurs,
                     agregats_data = filtredBas$agregats)
})
