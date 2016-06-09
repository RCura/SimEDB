output$polesNb <- renderPlot({
  
  PolesTous <- JIAP_poles %>%
    group_by(seed, Annee) %>%
    summarise(NbPoles = n()) %>%
    tbl_df()
  
  tempVar <- JIAP_poles %>%
    filter(Agregat != "null") %>%
    group_by(seed,Annee) %>%
    summarise(NbPoleAg = n())
  
  PolesAgregats <- PolesTous %>%
    left_join(tempVar, by=c("seed", "Annee")) %>%
    mutate(TxAg = NbPoleAg / NbPoles)
  
  tousPoles <- ggplot(data = PolesTous, aes(factor(Annee), NbPoles)) + 
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre de\npôles") +
    theme(axis.title.x=element_blank())
  
  polesAgregats <- ggplot(data = PolesAgregats, aes(factor(Annee), TxAg)) +
    geom_tufteboxplot() +
    scale_y_continuous(labels = scales::percent) +
    xlab("Temps") + ylab("Taux de pôles\ncontenant un agrégat") +
    theme(axis.title.x=element_blank())
  
  grid.arrange(tousPoles, polesAgregats, bottom = 'Temps')
  
})

output$polesNbFilter <- renderPlot({
  PolesTous <- filtred$poles %>%
    group_by(seed, Annee) %>%
    summarise(NbPoles = n()) %>%
    tbl_df()
  
  tempVar <- filtred$poles %>%
    filter(Agregat != "null") %>%
    group_by(seed,Annee) %>%
    summarise(NbPoleAg = n())
  
  PolesAgregats <- PolesTous %>%
    left_join(tempVar, by=c("seed", "Annee")) %>%
    mutate(TxAg = NbPoleAg / NbPoles)
  
  tousPoles <- ggplot(data = PolesTous, aes(factor(Annee), NbPoles)) + 
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre de\npôles") +
    theme(axis.title.x=element_blank())
  
  polesAgregats <- ggplot(data = PolesAgregats, aes(factor(Annee), TxAg)) +
    geom_tufteboxplot() +
    scale_y_continuous(labels = scales::percent) +
    xlab("Temps") + ylab("Taux de pôles\ncontenant un agrégat") +
    theme(axis.title.x=element_blank())
  
  grid.arrange(tousPoles, polesAgregats, bottom = 'Temps')
  
})

output$polesCompo <- renderPlot({
  poles_temps <- JIAP_poles %>%
    filter(Annee %in% c(820, 940, 1040, 1160))
  
  attracPoles <- poles_temps %>%
    mutate(`Attractivite` = replace(`Attractivite`, `Attractivite` == 0.68, 0.66)) %>%
    mutate(`Attractivite` = replace(`Attractivite`, `Attractivite` == 0.83, 0.85)) %>%
    group_by(seed, Annee,`Attractivite`) %>%
    summarise(Nb = n()) %>%
    tbl_df()
  
  
  compoPoles <- poles_temps %>%
    group_by(seed, Annee, NbAttracteurs) %>%
    summarise(Nb = n()) %>%
    tbl_df()
  
  plotAttrac <- ggplot(attracPoles, aes(factor(`Attractivite`), Nb)) +
    geom_tufteboxplot() + 
    facet_wrap(~Annee, scale="free", nrow = 1) +
    xlab("Attractivité") +
    theme(axis.title.y=element_blank()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  plotCompo <- ggplot(compoPoles, aes(factor(NbAttracteurs), Nb)) +
    geom_tufteboxplot() +
    facet_wrap(~Annee, scale="free", nrow=1) +
    xlab("Nombre d'attracteurs") +
    theme(axis.title.y=element_blank()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  grid.arrange(plotAttrac, plotCompo, nrow=2, top = "Évolution de la composition des pôles", left="Fréquence")
  
})

output$polesCompoFilter <- renderPlot({
  poles_temps <- filtred$poles %>%
    filter(Annee %in% c(820, 940, 1040, 1160))
  
  attracPoles <- poles_temps %>%
    mutate(`Attractivite` = replace(`Attractivite`, `Attractivite` == 0.68, 0.66)) %>%
    mutate(`Attractivite` = replace(`Attractivite`, `Attractivite` == 0.83, 0.85)) %>%
    group_by(seed, Annee,`Attractivite`) %>%
    summarise(Nb = n()) %>%
    tbl_df()
  
  
  compoPoles <- poles_temps %>%
    group_by(seed, Annee, NbAttracteurs) %>%
    summarise(Nb = n()) %>%
    tbl_df()
  
  plotAttrac <- ggplot(attracPoles, aes(factor(`Attractivite`), Nb)) +
    geom_tufteboxplot() + 
    facet_wrap(~Annee, scale="free", nrow = 1) +
    xlab("Attractivité") +
    theme(axis.title.y=element_blank()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  plotCompo <- ggplot(compoPoles, aes(factor(NbAttracteurs), Nb)) +
    geom_tufteboxplot() +
    facet_wrap(~Annee, scale="free", nrow=1) +
    xlab("Nombre d'attracteurs") +
    theme(axis.title.y=element_blank()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  grid.arrange(plotAttrac, plotCompo, nrow=2, top = "Évolution de la composition des pôles", left="Fréquence")
  
})

output$polesRT <- renderPlot({
  
  rtPoles_data <- JIAP_poles %>%
    group_by(seed, Annee) %>%
    mutate(Rank = row_number(desc(NbAttracteurs)))
  
  rtPoles_alpha <- rtPoles_data %>%
    split(list(.$seed, .$Annee)) %>%
    map(~ lm(log(NbAttracteurs) ~ log(Rank), data = .)) %>%
    map("coefficients") %>%
    map(function(x){return(x[2])}) %>%
    unlist() %>%
    as.data.frame(check.names = FALSE) %>%
    {colnames(.) <- "Value"; .} %>%
    mutate(ID = row.names(.)) %>%
    separate(col = ID, into = c("seed", "Annee", "var"), convert = TRUE) %>%
    select(-var) %>%
    mutate(seed = as.numeric(seed), Annee = as.numeric(Annee))
  
  ggplot(rtPoles_alpha, aes(factor(Annee), Value)) +
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Coefficient de la rang-taille") +
    ggtitle("Évolution de la pente\n de la courbe rang-taille\n des poles")
})

output$polesRTFilter <- renderPlot({
  
  rtPoles_data <- filtred$poles %>%
    group_by(seed, Annee) %>%
    mutate(Rank = row_number(desc(NbAttracteurs)))
  
  rtPoles_alpha <- rtPoles_data %>%
    split(list(.$seed, .$Annee)) %>%
    map(~ lm(log(NbAttracteurs) ~ log(Rank), data = .)) %>%
    map("coefficients") %>%
    map(function(x){return(x[2])}) %>%
    unlist() %>%
    as.data.frame(check.names = FALSE) %>%
    {colnames(.) <- "Value"; .} %>%
    mutate(ID = row.names(.)) %>%
    separate(col = ID, into = c("seed", "Annee", "var"), convert = TRUE) %>%
    select(-var) %>%
    mutate(seed = as.numeric(seed), Annee = as.numeric(Annee))
  
  ggplot(rtPoles_alpha, aes(factor(Annee), Value)) +
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Coefficient de la rang-taille") +
    ggtitle("Évolution de la pente\n de la courbe rang-taille\n des poles")
})

