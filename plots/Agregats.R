output$agregatsNb <- renderPlot({
  
  nombre_agregats <- JIAP_agregats %>% group_by(Annee, seed) %>% summarise(nb = n())
  
  ggplot(nombre_agregats, aes(factor(Annee), nb)) +
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre d'agrégats") +
    ggtitle("Évolution du nombre d'agrégats")
})

output$agregatsNbFilter <- renderPlot({
  nombre_agregats <- filtred$agregats %>% group_by(Annee, seed) %>% summarise(nb = n())
  
  ggplot(nombre_agregats, aes(factor(Annee), nb)) +
    geom_tufteboxplot() +
    xlab("Temps") + ylab("Nombre d'agrégats") +
    ggtitle("Évolution du nombre d'agrégats")
})

output$agregatsCompo <- renderPlot({
  agBreaks <- c(-1,29,49,99,1000)
  agLabels <- c("<30","30-49", "50-99", ">100")
  
  
  compoAgregats <- JIAP_agregats %>%
    filter(Annee %in% c(820, 940, 1040, 1160)) %>%
    mutate(NbFPBreaks =  cut(NbFpContenus, breaks = agBreaks, labels =  agLabels)) %>%
    group_by(seed, Annee, NbFPBreaks) %>%
    summarise(Freq = n()) %>%
    tbl_df()
  
  ggplot(compoAgregats, aes(NbFPBreaks,  Freq)) +
    geom_tufteboxplot() +
    facet_wrap(~Annee, scale="free", nrow=1) +
    xlab("Nombre de Foyers Paysans contenus") +
    ylab("Fréquence") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Évolution de la composition (en FP) des agrégats")
})

output$agregatsCompoFilter <- renderPlot({
  agBreaks <- c(-1,29,49,99,1000)
  agLabels <- c("<30","30-49", "50-99", ">100")
  
  
  compoAgregats <- filtred$agregats %>%
    filter(Annee %in% c(820, 940, 1040, 1160)) %>%
    mutate(NbFPBreaks =  cut(NbFpContenus, breaks = agBreaks, labels =  agLabels)) %>%
    group_by(seed, Annee, NbFPBreaks) %>%
    summarise(Freq = n()) %>%
    tbl_df()
  
  ggplot(compoAgregats, aes(NbFPBreaks,  Freq)) +
    geom_tufteboxplot() +
    facet_wrap(~Annee, scale="free", nrow=1) +
    xlab("Nombre de Foyers Paysans contenus") +
    ylab("Fréquence") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Évolution de la composition (en FP) des agrégats")
})

output$agregatsRT <- renderPlot({
  rtAgregats <- JIAP_agregats %>%
    filter(Annee %in% c(820, 940, 1040, 1160)) %>%
    group_by(seed, Annee) %>%
    mutate(Rank = row_number(desc(NbFContenusAvantDem))) %>%
    group_by(Annee, Rank) %>%
    summarise(Moyenne = mean(NbFContenusAvantDem), Q1 = quantile(NbFContenusAvantDem, probs=0.25), Q3 = quantile(NbFContenusAvantDem, probs=0.75)) %>%
    gather(key = `Méthode d'agrégation`, value = Value, Moyenne:Q3) %>%
    tbl_df() %>%
    ungroup() %>%
    mutate(Annee = Annee - 20)
  
  
  ggplot(rtAgregats, aes(Rank, Value, group=`Méthode d'agrégation`, colour=`Méthode d'agrégation`)) +
    geom_line(size=0.3, linetype="dotted") +
    geom_point(size=0.3) +
    scale_color_manual(values=c("black", "red", "blue")) +
    facet_grid(~Annee, space="free_x",  scale="free_x") +
    scale_x_log10() + scale_y_log10() +
    ggtitle("Évolution rang-taille de la composition des agrégats") +
    theme(legend.position="bottom") +
    xlab("Rang (log10)") + ylab("Nombre de FP\ncontenus (log10)")
})

output$agregatsRTFilter <- renderPlot({
  rtAgregats <- filtred$agregats %>%
    filter(Annee %in% c(820, 940, 1040, 1160)) %>%
    group_by(seed, Annee) %>%
    mutate(Rank = row_number(desc(NbFContenusAvantDem))) %>%
    group_by(Annee, Rank) %>%
    summarise(Moyenne = mean(NbFContenusAvantDem), Q1 = quantile(NbFContenusAvantDem, probs=0.25), Q3 = quantile(NbFContenusAvantDem, probs=0.75)) %>%
    gather(key = `Méthode d'agrégation`, value = Value, Moyenne:Q3) %>%
    tbl_df() %>%
    ungroup() %>%
    mutate(Annee = Annee - 20)
  
  
  
  ggplot(rtAgregats, aes(Rank, Value, group=`Méthode d'agrégation`, colour=`Méthode d'agrégation`)) +
    geom_line(size=0.3, linetype="dotted") +
    geom_point(size=0.3) +
    scale_color_manual(values=c("black", "red", "blue")) +
    facet_grid(~Annee, space="free_x",  scale="free_x") +
    scale_x_log10() + scale_y_log10() +
    ggtitle("Évolution rang-taille de la composition des agrégats") +
    theme(legend.position="bottom") +
    xlab("Rang (log10)") + ylab("Nombre de FP\ncontenus (log10)")
})