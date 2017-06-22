# output$fpDeplacements <- renderPlot({
#   gatherFP <- JIAP_FP_all %>%
#     group_by(Annee) %>%
#     summarise_each(funs(mean)) %>%
#     gather(Type, Value, nbInInIntra:nbOutOutInter) %>%
#     mutate(GrandType =  substr(Type, nchar(Type) - 4, nchar(Type))) %>%
#     mutate(Type = gsub("Inter", "", Type)) %>%
#     mutate(Type = gsub("Intra", "", Type)) %>%
#     mutate(Type = gsub("nb", "", Type))
#   
#   TypeDeplacement <- ggplot(gatherFP, aes(x=factor(Annee), y = Value)) +
#     geom_bar(stat = "identity", aes(fill=Type) ) +
#     facet_wrap(~GrandType) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#     theme(legend.position="bottom") +
#     xlab("Temps") + ylab("Nombre de Foyers Paysans") +
#     ggtitle("Type de déplacement des Foyers Paysans")
#   
#   TypeDeplacement
# })
# 
# output$fpDeplacementsFilter <- renderPlot({
#   gatherFP <- filtred$FP_all %>%
#     group_by(Annee) %>%
#     summarise_each(funs(mean)) %>%
#     gather(Type, Value, nbInInIntra:nbOutOutInter) %>%
#     mutate(GrandType =  substr(Type, nchar(Type) - 4, nchar(Type))) %>%
#     mutate(Type = gsub("Inter", "", Type)) %>%
#     mutate(Type = gsub("Intra", "", Type)) %>%
#     mutate(Type = gsub("nb", "", Type))
#   
#   TypeDeplacement <- ggplot(gatherFP, aes(x=factor(Annee), y = Value)) +
#     geom_bar(stat = "identity", aes(fill=Type) ) +
#     facet_wrap(~GrandType) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#     theme(legend.position="bottom") +
#     xlab("Temps") + ylab("Nombre de Foyers Paysans") +
#     ggtitle("Type de déplacement des Foyers Paysans")
#   
#   TypeDeplacement
# })


output$fpConcentration <- renderPlot({
  ggplot(sim_results, aes(factor(Annee), prop_FP_isoles)) +
    geom_tufteboxplot() +
    ggtitle("Évolution de la part de FP isolés") +
    xlab("Temps") + ylab("Taux de FP isolés") +
    scale_y_continuous(labels = scales::percent)
})

output$fpConcentrationFilter <- renderPlot({
  ggplot(filtred$results, aes(factor(Annee), prop_FP_isoles)) +
    geom_tufteboxplot() +
    ggtitle("Évolution de la part de FP isolés") +
    xlab("Temps") + ylab("Taux de FP isolés") +
    scale_y_continuous(labels = scales::percent)
})
# 
# output$fpSatisfaction <- renderPlot({
#   satisfaction_data <- JIAP_FP_summary %>%
#     filter(Annee %in% c(820, 940, 1040, 1160)) %>%
#     gather(ValSatisfaction, NbFP, `NbSat25inf`:`NbSat75sup`) %>%
#     mutate(ValSatisfaction = replace(ValSatisfaction, ValSatisfaction == "NbSat25inf", "<0.25")) %>%
#     mutate(ValSatisfaction = replace(ValSatisfaction, ValSatisfaction == "NbSat25_49", "0.25-0.49")) %>%
#     mutate(ValSatisfaction = replace(ValSatisfaction, ValSatisfaction == "NbSat50_75", "0.5-0.75")) %>%
#     mutate(ValSatisfaction = replace(ValSatisfaction, ValSatisfaction == "NbSat75sup", ">0.75")) %>%
#     mutate(ValSatisfaction = factor(ValSatisfaction)) #%>%
#   #mutate(ValSatisfaction = factor(ValSatisfaction, levels(ValSatisfaction)[c(1,3,4,2)])) #Windows 
#   
#   
#   ggplot(satisfaction_data, aes(ValSatisfaction, NbFP)) +
#     geom_tufteboxplot() +
#     facet_wrap(~ Annee, nrow=1) +
#     xlab("Satisfaction") + ylab("Nombre de FP") +
#     ggtitle("Évolution de la satisfaction des FP") +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
# })

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
  satisfaction_data <- filtred$sim_FP %>%
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