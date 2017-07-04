########################
########## FP ##########
########################

observeEvent(input$FP_TypeDeplacements_Rate,{
  req(input$FP_TypeDeplacements_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$FP_TypeDeplacements_Rate))[3],
                      input$FP_TypeDeplacements_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$FP_TypeDeplacements_Filter_Rate,{
  req(input$FP_TypeDeplacements_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$FP_TypeDeplacements_Filter_Rate))[3],
                      input$FP_TypeDeplacements_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$FP_DeplacementsDetail_Rate,{
  req(input$FP_DeplacementsDetail_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$FP_DeplacementsDetail_Rate))[3],
                      input$FP_DeplacementsDetail_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$FP_DeplacementsDetail_Filter_Rate,{
  req(input$FP_DeplacementsDetail_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$FP_DeplacementsDetail_Filter_Rate))[3],
                      input$FP_DeplacementsDetail_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$FP_Concentration_Rate,{
  req(input$FP_Concentration_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$FP_Concentration_Rate))[3],
                      input$FP_Concentration_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$FP_Concentration_Filter_Rate,{
  req(input$FP_Concentration_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$FP_Concentration_Filter_Rate))[3],
                      input$FP_Concentration_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$FP_Satisfaction_Rate,{
  req(input$FP_Satisfaction_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$FP_Satisfaction_Rate))[3],
                      input$FP_Satisfaction_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$FP_Satisfaction_Filter_Rate,{
  req(input$FP_Satisfaction_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$FP_Satisfaction_Filter_Rate))[3],
                      input$FP_Satisfaction_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

########################
####### Agrégats #######
########################

observeEvent(input$Agregats_Nb_Rate,{
  req(input$Agregats_Nb_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Agregats_Nb_Rate))[3],
                      input$Agregats_Nb_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Agregats_Nb_Filter_Rate,{
  req(input$Agregats_Nb_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Agregats_Nb_Filter_Rate))[3],
                      input$Agregats_Nb_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Agregats_Poles_Rate,{
  req(input$Agregats_Poles_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Agregats_Poles_Rate))[3],
                      input$Agregats_Poles_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Agregats_Poles_Filter_Rate,{
  req(input$Agregats_Poles_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Agregats_Poles_Filter_Rate))[3],
                      input$Agregats_Poles_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Agregats_CA_Rate,{
  req(input$Agregats_CA_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Agregats_CA_Rate))[3],
                      input$Agregats_CA_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Agregats_CA_Filter_Rate,{
  req(input$Agregats_CA_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Agregats_CA_Filter_Rate))[3],
                      input$Agregats_CA_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Agregats_RT_Rate,{
  req(input$Agregats_RT_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Agregats_RT_Rate))[3],
                      input$Agregats_RT_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Agregats_RT_Filter_Rate,{
  req(input$Agregats_RT_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Agregats_RT_Filter_Rate))[3],
                      input$Agregats_RT_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

#######################
###### Seigneurs ######
#######################

observeEvent(input$Seigneurs_Nb_Rate,{
  req(input$Seigneurs_Nb_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Nb_Rate))[3],
                      input$Seigneurs_Nb_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Nb_Filter_Rate,{
  req(input$Seigneurs_Nb_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Nb_Filter_Rate))[3],
                      input$Seigneurs_Nb_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Chateaux_Rate,{
  req(input$Seigneurs_Chateaux_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Chateaux_Rate))[3],
                      input$Seigneurs_Chateaux_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Chateaux_Filter_Rate,{
  req(input$Seigneurs_Chateaux_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Chateaux_Rate_Rate))[3],
                      input$Seigneurs_Chateaux_Rate_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Vassaux_Rate,{
  req(input$Seigneurs_Vassaux_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Vassaux_Rate))[3],
                      input$Seigneurs_Vassaux_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Vassaux_Filter_Rate,{
  req(input$Seigneurs_Vassaux_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Vassaux_Filter_Rate))[3],
                      input$Seigneurs_Vassaux_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Redevances_Rate,{
  req(input$Seigneurs_Redevances_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Redevances_Rate))[3],
                      input$Seigneurs_Redevances_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Redevances_Filter_Rate,{
  req(input$Seigneurs_Redevances_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Redevances_Filter_Rate))[3],
                      input$Seigneurs_Redevances_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Redevances_PS_Rate,{
  req(input$Seigneurs_Redevances_PS_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Redevances_PS_Rate))[3],
                      input$Seigneurs_Redevances_PS_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Redevances_PS_Filter_Rate,{
  req(input$Seigneurs_Redevances_PS_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Redevances_PS_Filter_Rate))[3],
                      input$Seigneurs_Redevances_PS_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Puissance_Rate,{
  req(input$Seigneurs_Puissance_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Puissance_Rate))[3],
                      input$Seigneurs_Puissance_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Puissance_Rate,{
  req(input$Seigneurs_Puissance_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Puissance_Filter_Rate))[3],
                      input$Seigneurs_Puissance_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Agregats_Rate,{
  req(input$Seigneurs_Agregats_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Agregats_Rate))[3],
                      input$Seigneurs_Agregats_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Agregats_Filter_Rate,{
  req(input$Seigneurs_Agregats_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Agregats_Filter_Rate))[3],
                      input$Seigneurs_Agregats_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)


###################
###### Poles ######
###################

observeEvent(input$Poles_Nb_Rate,{
  req(input$Poles_Nb_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Poles_Nb_Rate))[3],
                      input$Poles_Nb_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Poles_Nb_Filter_Rate,{
  req(input$Poles_Nb_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Poles_Nb_Filter_Rate))[3],
                      input$Poles_Nb_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Poles_Agregats_Rate,{
  req(input$Poles_Agregats_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Poles_Agregats_Rate))[3],
                      input$Poles_Agregats_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Poles_Agregats_Filter_Rate,{
  req(input$Poles_Agregats_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Poles_Agregats_Filter_Rate))[3],
                      input$Poles_Agregats_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Poles_Compo_Rate,{
  req(input$Poles_Compo_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Poles_Compo_Rate))[3],
                      input$Poles_Compo_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Poles_Compo_Filter_Rate,{
  req(input$Poles_Compo_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Poles_Compo_Filter_Rate))[3],
                      input$Poles_Compo_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Poles_Attrac_Rate,{
  req(input$Poles_Attrac_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Poles_Attrac_Rate))[3],
                      input$Poles_Attrac_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Poles_Attrac_Filter_Rate,{
  req(input$Poles_Attrac_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Poles_Attrac_Filter_Rate))[3],
                      input$Poles_Attrac_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Poles_RT_Rate,{
  req(input$Poles_RT_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Poles_RT_Rate))[3],
                      input$Poles_RT_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Poles_RT_Filter_Rate,{
  req(input$Poles_RT_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Poles_RT_Filter_Rate))[3],
                      input$Poles_RT_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

#######################
###### Paroisses ######
#######################

observeEvent(input$Paroisses_Nb_Rate,{
  req(input$Paroisses_Nb_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Paroisses_Nb_Rate))[3],
                      input$Paroisses_Nb_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Paroisses_Nb_Filter_Rate,{
  req(input$Paroisses_Nb_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Paroisses_Nb_Filter_Rate))[3],
                      input$Paroisses_Nb_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Paroisses_Compo_Rate,{
  req(input$Paroisses_Compo_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Paroisses_Compo_Rate))[3],
                      input$Paroisses_Compo_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Paroisses_Compo_Filter_Rate,{
  req(input$Paroisses_Compo_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Paroisses_Compo_Filter_Rate))[3],
                      input$Paroisses_Compo_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Paroisses_Promo_Rate,{
  req(input$Paroisses_Promo_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Paroisses_Promo_Rate))[3],
                      input$Paroisses_Promo_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Paroisses_Promo_Filter_Rate,{
  req(input$Paroisses_Promo_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Paroisses_Promo_Filter_Rate))[3],
                      input$Paroisses_Promo_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Paroisses_Superficie_Rate,{
  req(input$Paroisses_Superficie_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Paroisses_Superficie_Rate))[3],
                      input$Paroisses_Superficie_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Paroisses_Superficie_Filter_Rate,{
  req(input$Paroisses_Superficie_Filter_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Paroisses_Superficie_Filter_Rate))[3],
                      input$Paroisses_Superficie_Filter_Rate,
                      paste(as.character(filtredSeeds()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)