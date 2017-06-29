output$nbSeigneurs <- renderPlot({
  nbSeigneurs <- sim_seigneurs %>%
    filter(type != "Grand Seigneur") %>%
    group_by(seed, Annee, type) %>%
    summarise(N = n())
  
  ggplot(nbSeigneurs, aes(factor(Annee), col = type, fill = type, y = N)) +
    geom_tufteboxplot() +
    scale_color_discrete(name = "Type de seigneur") +
    scale_fill_discrete(name = "Type de seigneur") +
    ylab("Nombre de seigneurs") +
    xlab("Temps") +
    labs(title = "Évolution du nombre de seigneurs",
         subtitle = "Variabilité : Réplications")
})

output$nbSeigneursFilter <- renderPlot({
  req(filtred$seigneurs)
  
  nbSeigneurs <- filtred$seigneurs %>%
    filter(type != "Grand Seigneur") %>%
    group_by(seed, Annee, type) %>%
    summarise(N = n())
  
  ggplot(nbSeigneurs, aes(factor(Annee), col = type, fill = type, y = N)) +
    geom_tufteboxplot() +
    scale_color_discrete(name = "Type de seigneur") +
    scale_fill_discrete(name = "Type de seigneur") +
    ylab("Nombre de seigneurs") +
    xlab("Temps") +
    labs(title = "Évolution du nombre de seigneurs",
         subtitle = "Variabilité : Réplications")
})


output$seigneursChateaux <- renderPlot({
  
  breaksGS <- c(-0,1,2,3,4,5,10,25,50,1000)
  labelsGS <- c("1", "2", "3", "4", "5","6;10", "11;25", "26;50", ">50")
  
  # On ne garde que : les GS, les chatelains, et les PS/Chatelains inits
  
  GS <- sim_seigneurs %>%
    filter(Annee == 1160, type == "Grand Seigneur") %>%
    rename(`Propriétés` = nbChateauxProprio, Gardiennage = nbChateauxGardien) %>%
    gather(key = TypePossession, value = NbChateaux, `Propriétés`, Gardiennage) %>%
    xtabs(formula = ~ seed + type + TypePossession + NbChateaux) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    tbl_df() %>%
    mutate(NbChateaux = as.numeric(NbChateaux)) %>%
    filter(NbChateaux > 0) %>%
    mutate(NbChateauxBreaks =  cut(NbChateaux, breaks = breaksGS, labels =  labelsGS)) %>%
    group_by(seed, NbChateauxBreaks, TypePossession, type) %>%
    summarise(NbSeigneurs = sum(Freq)) %>%
    tbl_df()
  
  breaksChat <- c(-1,0,1,2,3,4,1000)
  labelsChat <- c("0", "1", "2", "3", "4", "5+")
  
  Chat <- sim_seigneurs %>%
    filter(Annee == 1160, type == "Chatelain") %>%
    rename(`Propriétés` = nbChateauxProprio, Gardiennage = nbChateauxGardien) %>%
    gather(key = TypePossession, value = NbChateaux, `Propriétés`, Gardiennage) %>%
    xtabs(formula = ~ seed + type + TypePossession + NbChateaux) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    tbl_df() %>%
    mutate(NbChateaux = as.numeric(NbChateaux)) %>%
    filter(NbChateaux > 0) %>%
    mutate(NbChateauxBreaks =  cut(NbChateaux, breaks = breaksChat, labels =  labelsChat)) %>%
    group_by(seed, NbChateauxBreaks, TypePossession, type) %>%
    summarise(NbSeigneurs = sum(Freq)) %>%
    tbl_df()
  
  plotGS <- ggplot(data = GS, aes(NbChateauxBreaks, NbSeigneurs)) +
    geom_tufteboxplot() +
    facet_wrap(~TypePossession, scale="free") +
    ggtitle("Grands Seigneurs") + 
    theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
    theme(plot.title = element_text(size = rel(1), face = "italic")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  plotChat <- ggplot(data = Chat, aes(NbChateauxBreaks, NbSeigneurs)) +
    geom_tufteboxplot() + 
    facet_wrap(~TypePossession, scale="free") +
    ggtitle("Chatelains") + 
    theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
    theme(plot.title = element_text(size = rel(1), face = "italic")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  grid.arrange(plotChat, plotGS, nrow=2,
               bottom = "Nombre de châteaux", left="Fréquence",
               top="Distribution des possessions \net gardiennages de châteaux")
})

output$seigneursChateauxFilter <- renderPlot({
  req(filtred$seigneurs)
  breaksGS <- c(-0,1,2,3,4,5,10,25,50,1000)
  labelsGS <- c("1", "2", "3", "4", "5","6;10", "11;25", "26;50", ">50")
  
  # On ne garde que : les GS, les chatelains, et les PS/Chatelains inits
  
  GS <- filtred$seigneurs %>%
    filter(Annee == 1160, Type == "Grand Seigneur") %>%
    rename(`Propriétés` = NbChateauxProprio, Gardiennage = NbChateauxGardien) %>%
    gather(key = TypePossession, value = NbChateaux, `Propriétés`, Gardiennage) %>%
    xtabs(formula = ~ seed + type + TypePossession + NbChateaux) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    tbl_df() %>%
    mutate(NbChateaux = as.numeric(NbChateaux)) %>%
    filter(NbChateaux > 0) %>%
    mutate(NbChateauxBreaks =  cut(NbChateaux, breaks = breaksGS, labels =  labelsGS)) %>%
    group_by(seed, NbChateauxBreaks, TypePossession, type) %>%
    summarise(NbSeigneurs = sum(Freq)) %>%
    tbl_df()
  
  breaksChat <- c(-1,0,1,2,3,4,1000)
  labelsChat<- c("0", "1", "2", "3", "4", "5+")
  
  Chat <- filtred$seigneurs %>%
    filter(Annee == 1160, type == "Chatelain") %>%
    rename(`Propriétés` = NbChateauxProprio, Gardiennage = NbChateauxGardien) %>%
    gather(key = TypePossession, value = NbChateaux, `Propriétés`, Gardiennage) %>%
    xtabs(formula = ~ seed + type + TypePossession + NbChateaux) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    tbl_df() %>%
    mutate(NbChateaux = as.numeric(NbChateaux)) %>%
    filter(NbChateaux > 0) %>%
    mutate(NbChateauxBreaks =  cut(NbChateaux, breaks = breaksChat, labels =  labelsChat)) %>%
    group_by(seed, NbChateauxBreaks, TypePossession, type) %>%
    summarise(NbSeigneurs = sum(Freq)) %>%
    tbl_df()
  
  plotGS <- ggplot(data = GS, aes(NbChateauxBreaks, NbSeigneurs)) +
    geom_tufteboxplot() +
    facet_wrap(~TypePossession, scale="free") +
    ggtitle("Grands Seigneurs") + 
    theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
    theme(plot.title = element_text(size = rel(1), face = "italic")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  plotChat <- ggplot(data = Chat, aes(NbChateauxBreaks, NbSeigneurs)) +
    geom_tufteboxplot() + 
    facet_wrap(~TypePossession, scale="free") +
    ggtitle("Chatelains") + 
    theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
    theme(plot.title = element_text(size = rel(1), face = "italic")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  grid.arrange(plotChat, plotGS, nrow=2,
               bottom = "Nombre de châteaux", left="Fréquence",
               top="Distribution des possessions \net gardiennages de châteaux")
})

output$seigneursVassaux <- renderPlot({
  myBreaks <- c(-1,0,1,2,3,4,5,10,25,50,1000)
  myLabels <- c("0","1", "2", "3", "4", "5","6;10", "11;25", "26;50", ">50")
  
  debiteurs_seigneurs<- sim_seigneurs %>%
    filter(Annee == 1160) %>%
    select(seed, Annee, type, initial, nbDebiteurs) %>%
    mutate(initial = replace(initial, initial=="true", "Initialement\nPrésent")) %>%
    mutate(initial = replace(initial, initial=="false", "Arrivé\nen cours")) %>%
    mutate(initial = factor(initial, levels = c("Arrivé\nen cours", "Initialement\nPrésent"))) %>%
    mutate(nbDebiteursBreaks =  cut(nbDebiteurs, breaks = myBreaks, labels =  myLabels)) %>%  
    group_by(seed, type, initial, nbDebiteursBreaks) %>%
    summarise(StatsDebiteurs = n())
  
  debInitCPS <- debiteurs_seigneurs %>% filter(type != "Grand Seigneur", initial == "Initialement\nPrésent")
  debNonCPS <- debiteurs_seigneurs %>% filter(type != "Grand Seigneur", initial == "Arrivé\nen cours")
  debGS <- debiteurs_seigneurs %>% filter(type == "Grand Seigneur")
  
  plotInitCPS <- ggplot(debInitCPS, aes(nbDebiteursBreaks, StatsDebiteurs)) +
    geom_tufteboxplot() +
    facet_grid(initial ~ type, scale="free", space = "free_x") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
    theme(plot.title = element_text(size = rel(1), face = "italic"))
  
  
  plotNonCPS <- ggplot(debNonCPS, aes(nbDebiteursBreaks, StatsDebiteurs)) +
    geom_tufteboxplot() +
    facet_grid(initial ~ type, scale="free", space = "free_x") +
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
  
  grid.arrange(plotNonCPS, plotInitCPS, plotGS, nrow=1, layout_matrix = lay,
               bottom="Nombre de Vassaux", left = "Fréquence",
               top ="Distribution du nombre\n de vassaux selon les types de seigneurs")
  
})

output$seigneursVassauxFilter <- renderPlot({
  req(filtred$seigneurs)
  
  myBreaks <- c(-1,0,1,2,3,4,5,10,25,50,1000)
  myLabels <- c("0","1", "2", "3", "4", "5","6;10", "11;25", "26;50", ">50")
  
  debiteurs_seigneurs<- filtred$seigneurs %>%
    filter(Annee == 1160) %>%
    select(seed, Annee, type, initial, nbDebiteurs) %>%
    mutate(initial = replace(initial, initial=="TRUE", "Initialement\nPrésent")) %>%
    mutate(initial = replace(initial, initial=="FALSE", "Arrivé\nen cours")) %>%
    mutate(initial = factor(initial, levels = c("Arrivé\nen cours", "Initialement\nPrésent"))) %>%
    mutate(nbDebiteursBreaks =  cut(nbDebiteurs, breaks = myBreaks, labels =  myLabels)) %>%  
    group_by(seed, type, initial, nbDebiteursBreaks) %>%
    summarise(StatsDebiteurs = n())
  
  debInitCPS <- debiteurs_seigneurs %>% filter(type != "Grand Seigneur", initial == "Initialement\nPrésent")
  debNonCPS <- debiteurs_seigneurs %>% filter(type != "Grand Seigneur", initial == "Arrivé\nen cours")
  debGS <- debiteurs_seigneurs %>% filter(type == "Grand Seigneur")
  
  plotInitCPS <- ggplot(debInitCPS, aes(nbDebiteursBreaks, StatsDebiteurs)) +
    geom_tufteboxplot() +
    facet_grid(initial ~ type, scale="free", space = "free_x") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
    theme(plot.title = element_text(size = rel(1), face = "italic"))
  
  
  plotNonCPS <- ggplot(debNonCPS, aes(nbDebiteursBreaks, StatsDebiteurs)) +
    geom_tufteboxplot() +
    facet_grid(initial ~ type, scale="free", space = "free_x") +
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
  
  grid.arrange(plotNonCPS, plotInitCPS, plotGS, nrow=1, layout_matrix = lay,
               bottom="Nombre de Vassaux", left = "Fréquence",
               top ="Distribution du nombre\n de vassaux selon les types de seigneurs")
  
})

output$seigneursRedevances <- renderPlot({
  redevances_seigneurs <- sim_seigneurs %>%
    filter(Annee == 1160) %>%
    select(seed, Annee, type, nbFPassujettis) %>%
    mutate(type = factor(type, levels = c("Petit Seigneur", "Chatelain", "Grand Seigneur")))
  
  ggplot(redevances_seigneurs, aes(type, nbFPassujettis)) +
    geom_tufteboxplot() +
    scale_y_log10(breaks = c(10,50,100, 500,1000, 2000)) +
    xlab("Types de seigneurs") + ylab("Nombre de FP assujetis\n(Échelle logarithmique)") +
    ggtitle("Distribution des redevances en fin de simulation") +
    labs(subtitle = "Variabilité : Seigneurs et réplications")
})

output$seigneursRedevancesFilter <- renderPlot({
  req(filtred$seigneurs)
  
  redevances_seigneurs <- filtred$seigneurs %>%
    filter(Annee == 1160) %>%
    select(seed, Annee, type, nbFPassujettis) %>%
    mutate(type = factor(type, levels = c("Petit Seigneur", "Chatelain", "Grand Seigneur")))
  
  ggplot(redevances_seigneurs, aes(type, nbFPassujettis)) +
    geom_tufteboxplot() +
    scale_y_log10(breaks = c(10,50,100, 500,1000, 2000)) +
    xlab("Types de seigneurs") + ylab("Nombre de FP assujetis\n(Échelle logarithmique)") +
    ggtitle("Distribution des redevances en fin de simulation") +
    labs(subtitle = "Variabilité : Seigneurs et réplications")
})

output$PSredevances <- renderPlot({
  
  redevancesLevels <- c("0","1-5","6-15","16-30","30-100",">100")
  redevancesBreaks <- rlang::exprs(
    .data[[x]] == 0 ~ "0",
    .data[[x]] <= 5 ~ "1-5",
    .data[[x]] <= 15 ~ "6-15",
    .data[[x]] <= 30 ~ "16-30",
    .data[[x]] <= 100 ~ "30-100",
    .data[[x]] > 100 ~ ">100"
  )
  
  x <- "nbFPassujettis"
  
  redevances_PS <- sim_seigneurs %>%
    filter(Annee == 1160) %>%
    filter(type != "Grand Seigneur") %>%
    select(seed, Annee, type, nbFPassujettis) %>%
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
})

output$PSredevancesFilter <- renderPlot({
  req(filtred$seigneurs)
  
  redevancesLevels <- c("0","1-5","6-15","16-30","30-100",">100")
  redevancesBreaks <- rlang::exprs(
    .data[[x]] == 0 ~ "0",
    .data[[x]] <= 5 ~ "1-5",
    .data[[x]] <= 15 ~ "6-15",
    .data[[x]] <= 30 ~ "16-30",
    .data[[x]] <= 100 ~ "30-100",
    .data[[x]] > 100 ~ ">100"
  )
  
  x <- "nbFPassujettis"
  
  redevances_PS <- filtred$seigneurs %>%
    filter(Annee == 1160) %>%
    filter(type != "Grand Seigneur") %>%
    select(seed, Annee, type, nbFPassujettis) %>%
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
})


output$seigneursPuissance <- renderPlot({
  
  seigneurs_puissance <- sim_seigneurs %>%
    filter(puissance > 0) %>%
    group_by(seed, Annee, type) %>%
    summarise(Q1 =quantile(puissance, 0.25), Mean = mean(puissance), Max = max(puissance)) %>%
    gather(TypeIndic, puissance, Q1:Max)
  
  
  ggplot(seigneurs_puissance, aes(factor(Annee), puissance)) +geom_tufteboxplot() +
    facet_grid(type~TypeIndic, scale="free") +
    xlab("Temps") + ylab("Puissance") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Évolution de puissance des seigneurs\n(Puissance > 0, ≈50% des seigneurs)")
})

output$seigneursPuissanceFilter <- renderPlot({
  req(filtred$seigneurs)
  
  seigneurs_puissance <- filtred$seigneurs %>%
    filter(puissance > 0) %>%
    group_by(seed, Annee, type) %>%
    summarise(Q1 =quantile(puissance, 0.25), Mean = mean(puissance), Max = max(puissance)) %>%
    gather(TypeIndic, puissance, Q1:Max)
  
  
  ggplot(seigneurs_puissance, aes(factor(Annee), puissance)) +geom_tufteboxplot() +
    facet_grid(type~TypeIndic, scale="free") +
    xlab("Temps") + ylab("Puissance") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Évolution de puissance des seigneurs\n(Puissance > 0, ≈50% des seigneurs)")
})