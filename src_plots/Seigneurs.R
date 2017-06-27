output$seigneursChateaux <- renderPlot({
  
  breaksGS <- c(-0,1,2,3,4,5,10,25,50,1000)
  labelsGS <- c("1", "2", "3", "4", "5","6;10", "11;25", "26;50", ">50")
  
  # On ne garde que : les GS, les chatelains, et les PS/Chatelains inits
  
  GS <- JIAP_seigneurs %>%
    filter(Annee == 1160, Type == "Grand Seigneur") %>%
    rename(`Propriétés` = NbChateauxProprio, Gardiennage = NbChateauxGardien) %>%
    gather(key = TypePossession, value = NbChateaux, `Propriétés`, Gardiennage) %>%
    xtabs(formula = ~ seed + Type + TypePossession + NbChateaux) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    tbl_df() %>%
    mutate(NbChateaux = as.numeric(NbChateaux)) %>%
    filter(NbChateaux > 0) %>%
    mutate(NbChateauxBreaks =  cut(NbChateaux, breaks = breaksGS, labels =  labelsGS)) %>%
    group_by(seed, NbChateauxBreaks, TypePossession, Type) %>%
    summarise(NbSeigneurs = sum(Freq)) %>%
    tbl_df()
  
  breaksChat <- c(-1,0,1,2,3,4,1000)
  labelsChat<- c("0", "1", "2", "3", "4", "5+")
  
  Chat <- JIAP_seigneurs %>%
    filter(Annee == 1160, Type == "Chatelain") %>%
    rename(`Propriétés` = NbChateauxProprio, Gardiennage = NbChateauxGardien) %>%
    gather(key = TypePossession, value = NbChateaux, `Propriétés`, Gardiennage) %>%
    xtabs(formula = ~ seed + Type + TypePossession + NbChateaux) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    tbl_df() %>%
    mutate(NbChateaux = as.numeric(NbChateaux)) %>%
    filter(NbChateaux > 0) %>%
    mutate(NbChateauxBreaks =  cut(NbChateaux, breaks = breaksChat, labels =  labelsChat)) %>%
    group_by(seed, NbChateauxBreaks, TypePossession, Type) %>%
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
  
  breaksGS <- c(-0,1,2,3,4,5,10,25,50,1000)
  labelsGS <- c("1", "2", "3", "4", "5","6;10", "11;25", "26;50", ">50")
  
  # On ne garde que : les GS, les chatelains, et les PS/Chatelains inits
  
  GS <- filtred$seigneurs %>%
    filter(Annee == 1160, Type == "Grand Seigneur") %>%
    rename(`Propriétés` = NbChateauxProprio, Gardiennage = NbChateauxGardien) %>%
    gather(key = TypePossession, value = NbChateaux, `Propriétés`, Gardiennage) %>%
    xtabs(formula = ~ seed + Type + TypePossession + NbChateaux) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    tbl_df() %>%
    mutate(NbChateaux = as.numeric(NbChateaux)) %>%
    filter(NbChateaux > 0) %>%
    mutate(NbChateauxBreaks =  cut(NbChateaux, breaks = breaksGS, labels =  labelsGS)) %>%
    group_by(seed, NbChateauxBreaks, TypePossession, Type) %>%
    summarise(NbSeigneurs = sum(Freq)) %>%
    tbl_df()
  
  breaksChat <- c(-1,0,1,2,3,4,1000)
  labelsChat<- c("0", "1", "2", "3", "4", "5+")
  
  Chat <- filtred$seigneurs %>%
    filter(Annee == 1160, Type == "Chatelain") %>%
    rename(`Propriétés` = NbChateauxProprio, Gardiennage = NbChateauxGardien) %>%
    gather(key = TypePossession, value = NbChateaux, `Propriétés`, Gardiennage) %>%
    xtabs(formula = ~ seed + Type + TypePossession + NbChateaux) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    tbl_df() %>%
    mutate(NbChateaux = as.numeric(NbChateaux)) %>%
    filter(NbChateaux > 0) %>%
    mutate(NbChateauxBreaks =  cut(NbChateaux, breaks = breaksChat, labels =  labelsChat)) %>%
    group_by(seed, NbChateauxBreaks, TypePossession, Type) %>%
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
  
  debiteurs_seigneurs<- JIAP_seigneurs %>%
    filter(Annee == 1160) %>%
    select(seed, Annee, Type, Initial, NbDebiteurs) %>%
    mutate(Initial= as.character(Initial)) %>%
    mutate(Initial = replace(Initial, Initial=="TRUE", "Initialement\nPrésent")) %>%
    mutate(Initial = replace(Initial, Initial=="FALSE", "Arrivé\nen cours")) %>%
    mutate(Initial = factor(Initial, levels = c("Arrivé\nen cours", "Initialement\nPrésent"))) %>%
    mutate(NbDebiteursBreaks =  cut(NbDebiteurs, breaks = myBreaks, labels =  myLabels)) %>%  
    group_by(seed, Type, Initial, NbDebiteursBreaks) %>%
    summarise(StatsDebiteurs = n())
  
  debInitCPS <- debiteurs_seigneurs %>% filter(Type != "Grand Seigneur", Initial == "Initialement\nPrésent")
  debNonCPS <- debiteurs_seigneurs %>% filter(Type != "Grand Seigneur", Initial == "Arrivé\nen cours")
  debGS <- debiteurs_seigneurs %>% filter(Type == "Grand Seigneur")
  
  plotInitCPS <- ggplot(debInitCPS, aes(NbDebiteursBreaks, StatsDebiteurs)) +
    geom_tufteboxplot() +
    facet_grid(Initial ~ Type, scale="free", space = "free_x") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
    theme(plot.title = element_text(size = rel(1), face = "italic"))
  
  
  plotNonCPS <- ggplot(debNonCPS, aes(NbDebiteursBreaks, StatsDebiteurs)) +
    geom_tufteboxplot() +
    facet_grid(Initial ~ Type, scale="free", space = "free_x") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
    theme(plot.title = element_text(size = rel(1), face = "italic"))
  
  
  plotGS <- ggplot(debGS, aes(NbDebiteursBreaks, StatsDebiteurs)) +
    geom_tufteboxplot() +
    facet_wrap(~Type) +
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
  myBreaks <- c(-1,0,1,2,3,4,5,10,25,50,1000)
  myLabels <- c("0","1", "2", "3", "4", "5","6;10", "11;25", "26;50", ">50")
  
  debiteurs_seigneurs<- filtred$seigneurs %>%
    filter(Annee == 1160) %>%
    select(seed, Annee, Type, Initial, NbDebiteurs) %>%
    mutate(Initial= as.character(Initial)) %>%
    mutate(Initial = replace(Initial, Initial=="TRUE", "Initialement\nPrésent")) %>%
    mutate(Initial = replace(Initial, Initial=="FALSE", "Arrivé\nen cours")) %>%
    mutate(Initial = factor(Initial, levels = c("Arrivé\nen cours", "Initialement\nPrésent"))) %>%
    mutate(NbDebiteursBreaks =  cut(NbDebiteurs, breaks = myBreaks, labels =  myLabels)) %>%  
    group_by(seed, Type, Initial, NbDebiteursBreaks) %>%
    summarise(StatsDebiteurs = n())
  
  debInitCPS <- debiteurs_seigneurs %>% filter(Type != "Grand Seigneur", Initial == "Initialement\nPrésent")
  debNonCPS <- debiteurs_seigneurs %>% filter(Type != "Grand Seigneur", Initial == "Arrivé\nen cours")
  debGS <- debiteurs_seigneurs %>% filter(Type == "Grand Seigneur")
  
  plotInitCPS <- ggplot(debInitCPS, aes(NbDebiteursBreaks, StatsDebiteurs)) +
    geom_tufteboxplot() +
    facet_grid(Initial ~ Type, scale="free", space = "free_x") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
    theme(plot.title = element_text(size = rel(1), face = "italic"))
  
  
  plotNonCPS <- ggplot(debNonCPS, aes(NbDebiteursBreaks, StatsDebiteurs)) +
    geom_tufteboxplot() +
    facet_grid(Initial ~ Type, scale="free", space = "free_x") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
    theme(plot.title = element_text(size = rel(1), face = "italic"))
  
  
  plotGS <- ggplot(debGS, aes(NbDebiteursBreaks, StatsDebiteurs)) +
    geom_tufteboxplot() +
    facet_wrap(~Type) +
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
  redevances_seigneurs <- JIAP_seigneurs %>%
    filter(Annee == 1160) %>%
    select(seed, Annee, Type, Initial, NbFpAssujetis) %>%
    mutate(Initial = replace(Initial, Initial == "true", "Initialement\nPrésent")) %>%
    mutate(Initial = replace(Initial, Initial == "false", "Arrivé\nen cours")) %>%
    mutate(Initial = factor(Initial, levels = c("Arrivé\nen cours", "Initialement\nPrésent"))) %>%
    mutate(Type = factor(Type, levels = c("Petit Seigneur", "Chatelain", "Grand Seigneur"))) %>%
    group_by(seed, Type, Initial) %>%
    summarise(Redevances = mean(NbFpAssujetis))
  
  ggplot(redevances_seigneurs, aes(Redevances)) +
    geom_histogram(bins=20) + 
    facet_grid(Initial ~ Type, scales = "free") +
    ylab("Fréquence") + xlab("Nombre de FP assujetis") +
    ggtitle("Distribution des redevances")
})

output$seigneursRedevancesFilter <- renderPlot({
  redevances_seigneurs <- filtred$seigneurs %>%
    filter(Annee == 1160) %>%
    select(seed, Annee, Type, Initial, NbFpAssujetis) %>%
    mutate(Initial = replace(Initial, Initial == "true", "Initialement\nPrésent")) %>%
    mutate(Initial = replace(Initial, Initial == "false", "Arrivé\nen cours")) %>%
    mutate(Initial = factor(Initial, levels = c("Arrivé\nen cours", "Initialement\nPrésent"))) %>%
    mutate(Type = factor(Type, levels = c("Petit Seigneur", "Chatelain", "Grand Seigneur"))) %>%
    group_by(seed, Type, Initial) %>%
    summarise(Redevances = mean(NbFpAssujetis))
  
  ggplot(redevances_seigneurs, aes(Redevances)) +
    geom_histogram(bins=20) + 
    facet_grid(Initial ~ Type, scales = "free") +
    ylab("Fréquence") + xlab("Nombre de FP assujetis") +
    ggtitle("Distribution des redevances")
})

output$seigneursPuissance <- renderPlot({
  
  seigneurs_puissance <- JIAP_seigneurs %>%
    filter(puissance > 0) %>%
    group_by(seed, Annee, Type) %>%
    summarise(Q1 =quantile(puissance, 0.25), Mean = mean(puissance), Max = max(puissance)) %>%
    gather(TypeIndic, puissance, Q1:Max)
  
  
  ggplot(seigneurs_puissance, aes(factor(Annee), puissance)) +geom_tufteboxplot() +
    facet_grid(Type~TypeIndic, scale="free") +
    xlab("Temps") + ylab("Puissance") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Évolution de puissance des seigneurs\n(Puissance > 0, ≈50% des seigneurs)")
})

output$seigneursPuissanceFilter <- renderPlot({
  
  seigneurs_puissance <- filtred$seigneurs %>%
    filter(puissance > 0) %>%
    group_by(seed, Annee, Type) %>%
    summarise(Q1 =quantile(puissance, 0.25), Mean = mean(puissance), Max = max(puissance)) %>%
    gather(TypeIndic, puissance, Q1:Max)
  
  
  ggplot(seigneurs_puissance, aes(factor(Annee), puissance)) +geom_tufteboxplot() +
    facet_grid(Type~TypeIndic, scale="free") +
    xlab("Temps") + ylab("Puissance") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Évolution de puissance des seigneurs\n(Puissance > 0, ≈50% des seigneurs)")
})