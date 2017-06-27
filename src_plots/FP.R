output$fpDeplacementsDetail <- renderPlot({
  details_deplacement <- sim_FP %>%
    filter(Annee %in% c(820, 940, 1040, 1160)) %>%
    group_by(seed, Annee, deplacement_from, deplacement_to) %>%
    summarise(NbFP = n()) %>%
    group_by(Annee, deplacement_from, deplacement_to) %>%
    summarise(NbFP = mean(NbFP, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(deplacement_from != "nil") %>%
    filter(deplacement_to != "nil") %>%
    mutate(deplacement_from = gsub(x = deplacement_from, pattern = "agregat", replacement = "Origine : Agrégat")) %>%
    mutate(deplacement_from = gsub(x = deplacement_from, pattern = "isole", replacement = "Origine : Isolé")) %>%
    mutate(deplacement_to = gsub(x = deplacement_to, pattern = "agregat", replacement = "agrégat")) %>%
    mutate(deplacement_to = gsub(x = deplacement_to, pattern = "pole", replacement = "pôle")) %>%
    mutate(deplacement_to = paste0(toupper(substr(deplacement_to, 1, 1)), substr(deplacement_to, 2, nchar(deplacement_to))))
  
  
  ggplot(details_deplacement, aes(Annee, NbFP, fill = deplacement_to, group = deplacement_to)) + 
    geom_col(position = "dodge") +
    facet_wrap(~deplacement_from, ncol = 1) +
    xlab("Temps") + ylab("Nombre (moyen) de Foyers Paysans") +
    ggtitle("Détail du type de déplacement des Foyers Paysans") +
    scale_fill_discrete(name = "Choix de la destination") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "bottom")
  
})

output$fpTypeDeplacements <- renderPlot({
  types_deplacements <- sim_FP %>%
    group_by(Annee, seed, type_deplacement) %>%
    summarise(N = n()) %>%
    group_by(Annee, seed) %>%
    mutate(Tx = N / sum(N)) %>%
    filter(type_deplacement != "nil")
  
  ggplot(types_deplacements, aes(Annee, Tx, col = type_deplacement)) +
    geom_smooth() +
    scale_y_continuous(labels = percent) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position="bottom") +
    xlab("Temps") + ylab("Part des Foyers Paysans") +
    ggtitle("Type de déplacement des Foyers Paysans")
})

output$fpTypeDeplacementsFilter <- renderPlot({
  req(filtred$FP)
  types_deplacements <- filtred$sim_FP %>%
    group_by(Annee, seed, type_deplacement) %>%
    summarise(N = n()) %>%
    group_by(Annee, seed) %>%
    mutate(Tx = N / sum(N)) %>%
    filter(type_deplacement != "nil")
  
  ggplot(types_deplacements, aes(Annee, Tx, col = type_deplacement)) +
    geom_smooth() +
    scale_y_continuous(labels = percent) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position="bottom") +
    xlab("Temps") + ylab("Part des Foyers Paysans") +
    ggtitle("Type de déplacement des Foyers Paysans")
})

output$fpDeplacementsFilter <- renderPlot({
  req(filtred$FP)
  gatherFP <- filtred$FP %>%
    group_by(Annee) %>%
    summarise_each(funs(mean)) %>%
    gather(Type, Value, nbInInIntra:nbOutOutInter) %>%
    mutate(GrandType =  substr(Type, nchar(Type) - 4, nchar(Type))) %>%
    mutate(Type = gsub("Inter", "", Type)) %>%
    mutate(Type = gsub("Intra", "", Type)) %>%
    mutate(Type = gsub("nb", "", Type))

  TypeDeplacement <- ggplot(gatherFP, aes(x=factor(Annee), y = Value)) +
    geom_bar(stat = "identity", aes(fill=Type) ) +
    facet_wrap(~GrandType) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position="bottom") +
    xlab("Temps") + ylab("Nombre de Foyers Paysans") +
    ggtitle("Type de déplacement des Foyers Paysans")

  TypeDeplacement
})


output$fpConcentration <- renderPlot({
  ggplot(sim_results, aes(factor(Annee), prop_FP_isoles)) +
    geom_tufteboxplot() +
    ggtitle("Évolution de la part de FP isolés") +
    xlab("Temps") + ylab("Taux de FP isolés") +
    scale_y_continuous(labels = scales::percent)
})

output$fpConcentrationFilter <- renderPlot({
  req(filtred$results)
  ggplot(filtred$results, aes(factor(Annee), prop_FP_isoles)) +
    geom_tufteboxplot() +
    ggtitle("Évolution de la part de FP isolés") +
    xlab("Temps") + ylab("Taux de FP isolés") +
    scale_y_continuous(labels = scales::percent)
})


output$fpSatisfaction <- renderPlot({
  satisfaction_data <- sim_FP %>%
      select(Annee, sMat, sRel, sProt, Satis) %>%
      rename(
        Globale = Satis,
        Matérielle = sMat,
        Protection = sProt,
        Religieuse = sRel) %>%
      group_by(Annee) %>%
      sample_n(size = 4E3, replace = FALSE) %>%
      ungroup() %>%
      gather(key = Type, value = Satisfaction, -Annee)
  
  
  ggplot(satisfaction_data, aes(Annee, Satisfaction, col = Type, fill = Type)) +
    geom_violin(aes(group = factor(Annee))) +
    facet_wrap(~ Type) +
    geom_smooth(alpha = .3, se = FALSE, na.rm = TRUE) +
    theme(legend.position = "none") +
    ggtitle("Évolution de la satisfaction des FP\n(Échantillon de 4000 FP / an)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
   
})

output$fpSatisfactionFilter <- renderPlot({
  req(filtred$FP)
  satisfaction_data <- filtred$FP %>%
      select(Annee, sMat, sRel, sProt, Satis) %>%
      rename(
        Globale = Satis,
        Matérielle = sMat,
        Protection = sProt,
        Religieuse = sRel) %>%
      group_by(Annee) %>%
      sample_n(size = 4E3, replace = FALSE) %>%
      ungroup() %>%
      gather(key = Type, value = Satisfaction, -Annee)
    
    
    ggplot(satisfaction_data, aes(Annee, Satisfaction, col = Type, fill = Type)) +
      geom_violin(aes(group = factor(Annee))) +
      facet_wrap(~ Type) +
      geom_smooth(alpha = .3, se = FALSE, na.rm = TRUE) +
      theme(legend.position = "none") +
      ggtitle("Évolution de la satisfaction des FP\n(Échantillon de 4000 FP / an)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
})