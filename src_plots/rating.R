########################
########## FP ##########
########################

observeEvent(input$FP_TypeDeplacements_Haut_Rate,{
  req(input$FP_TypeDeplacements_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$FP_TypeDeplacements_Haut_Rate))[3],
                      input$FP_TypeDeplacements_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$FP_TypeDeplacements_Bas_Rate,{
  req(input$FP_TypeDeplacements_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$FP_TypeDeplacements_Bas_Rate))[3],
                      input$FP_TypeDeplacements_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$FP_DeplacementsDetail_Haut_Rate,{
  req(input$FP_DeplacementsDetail_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$FP_DeplacementsDetail_Haut_Rate))[3],
                      input$FP_DeplacementsDetail_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$FP_DeplacementsDetail_Bas_Rate,{
  req(input$FP_DeplacementsDetail_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$FP_DeplacementsDetail_Bas_Rate))[3],
                      input$FP_DeplacementsDetail_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$FP_Concentration_Haut_Rate,{
  req(input$FP_Concentration_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$FP_Concentration_Haut_Rate))[3],
                      input$FP_Concentration_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$FP_Concentration_Bas_Rate,{
  req(input$FP_Concentration_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$FP_Concentration_Bas_Rate))[3],
                      input$FP_Concentration_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$FP_Satisfaction_Haut_Rate,{
  req(input$FP_Satisfaction_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$FP_Satisfaction_Haut_Rate))[3],
                      input$FP_Satisfaction_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$FP_Satisfaction_Bas_Rate,{
  req(input$FP_Satisfaction_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$FP_Satisfaction_Bas_Rate))[3],
                      input$FP_Satisfaction_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

########################
####### Agrégats #######
########################

observeEvent(input$Agregats_Nb_Haut_Rate,{
  req(input$Agregats_Nb_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Agregats_Nb_Haut_Rate))[3],
                      input$Agregats_Nb_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Agregats_Nb_Bas_Rate,{
  req(input$Agregats_Nb_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Agregats_Nb_Bas_Rate))[3],
                      input$Agregats_Nb_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Agregats_Poles_Haut_Rate,{
  req(input$Agregats_Poles_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Agregats_Poles_Haut_Rate))[3],
                      input$Agregats_Poles_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Agregats_Poles_Bas_Rate,{
  req(input$Agregats_Poles_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Agregats_Poles_Bas_Rate))[3],
                      input$Agregats_Poles_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Agregats_Paroisses_Haut_Rate,{
  req(input$Agregats_Paroisses_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Agregats_Paroisses_Haut_Rate))[3],
                      input$Agregats_Paroisses_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
}
)

observeEvent(input$Agregats_Paroisses_Bas_Rate,{
  req(input$Agregats_Paroisses_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Agregats_Paroisses_Bas_Rate))[3],
                      input$Agregats_Paroisses_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
}
)

observeEvent(input$Agregats_CA_Haut_Rate,{
  req(input$Agregats_CA_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Agregats_CA_Haut_Rate))[3],
                      input$Agregats_CA_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Agregats_CA_Bas_Rate,{
  req(input$Agregats_CA_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Agregats_CA_Bas_Rate))[3],
                      input$Agregats_CA_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Agregats_RT_Haut_Rate,{
  req(input$Agregats_RT_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Agregats_RT_Haut_Rate))[3],
                      input$Agregats_RT_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Agregats_RT_Bas_Rate,{
  req(input$Agregats_RT_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Agregats_RT_Bas_Rate))[3],
                      input$Agregats_RT_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

#######################
###### Seigneurs ######
#######################

observeEvent(input$Seigneurs_Nb_Haut_Rate,{
  req(input$Seigneurs_Nb_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Nb_Haut_Rate))[3],
                      input$Seigneurs_Nb_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Nb_Bas_Rate,{
  req(input$Seigneurs_Nb_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Nb_Bas_Rate))[3],
                      input$Seigneurs_Nb_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Chateaux_Haut_Rate,{
  req(input$Seigneurs_Chateaux_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Chateaux_Haut_Rate))[3],
                      input$Seigneurs_Chateaux_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Chateaux_Bas_Rate,{
  req(input$Seigneurs_Chateaux_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Chateaux_Haut_Rate_Haut_Rate))[3],
                      input$Seigneurs_Chateaux_Haut_Rate_Haut_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Vassaux_Haut_Rate,{
  req(input$Seigneurs_Vassaux_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Vassaux_Haut_Rate))[3],
                      input$Seigneurs_Vassaux_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Vassaux_Bas_Rate,{
  req(input$Seigneurs_Vassaux_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Vassaux_Bas_Rate))[3],
                      input$Seigneurs_Vassaux_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Redevances_Haut_Rate,{
  req(input$Seigneurs_Redevances_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Redevances_Haut_Rate))[3],
                      input$Seigneurs_Redevances_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Redevances_Bas_Rate,{
  req(input$Seigneurs_Redevances_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Redevances_Bas_Rate))[3],
                      input$Seigneurs_Redevances_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Redevances_PS_Haut_Rate,{
  req(input$Seigneurs_Redevances_PS_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Redevances_PS_Haut_Rate))[3],
                      input$Seigneurs_Redevances_PS_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Redevances_PS_Bas_Rate,{
  req(input$Seigneurs_Redevances_PS_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Redevances_PS_Bas_Rate))[3],
                      input$Seigneurs_Redevances_PS_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Puissance_Haut_Rate,{
  req(input$Seigneurs_Puissance_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Puissance_Haut_Rate))[3],
                      input$Seigneurs_Puissance_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Puissance_Bas_Rate,{
  req(input$Seigneurs_Puissance_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Puissance_Bas_Rate))[3],
                      input$Seigneurs_Puissance_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Agregats_Haut_Rate,{
  req(input$Seigneurs_Agregats_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Agregats_Haut_Rate))[3],
                      input$Seigneurs_Agregats_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Seigneurs_Agregats_Bas_Rate,{
  req(input$Seigneurs_Agregats_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Seigneurs_Agregats_Bas_Rate))[3],
                      input$Seigneurs_Agregats_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)


###################
###### Poles ######
###################

observeEvent(input$Poles_Nb_Haut_Rate,{
  req(input$Poles_Nb_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Poles_Nb_Haut_Rate))[3],
                      input$Poles_Nb_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Poles_Nb_Bas_Rate,{
  req(input$Poles_Nb_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Poles_Nb_Bas_Rate))[3],
                      input$Poles_Nb_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Poles_Agregats_Haut_Rate,{
  req(input$Poles_Agregats_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Poles_Agregats_Haut_Rate))[3],
                      input$Poles_Agregats_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Poles_Agregats_Bas_Rate,{
  req(input$Poles_Agregats_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Poles_Agregats_Bas_Rate))[3],
                      input$Poles_Agregats_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Poles_Compo_Haut_Rate,{
  req(input$Poles_Compo_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Poles_Compo_Haut_Rate))[3],
                      input$Poles_Compo_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Poles_Compo_Bas_Rate,{
  req(input$Poles_Compo_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Poles_Compo_Bas_Rate))[3],
                      input$Poles_Compo_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Poles_Attrac_Haut_Rate,{
  req(input$Poles_Attrac_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Poles_Attrac_Haut_Rate))[3],
                      input$Poles_Attrac_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Poles_Attrac_Bas_Rate,{
  req(input$Poles_Attrac_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Poles_Attrac_Bas_Rate))[3],
                      input$Poles_Attrac_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Poles_RT_Haut_Rate,{
  req(input$Poles_RT_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Poles_RT_Haut_Rate))[3],
                      input$Poles_RT_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Poles_RT_Bas_Rate,{
  req(input$Poles_RT_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Poles_RT_Bas_Rate))[3],
                      input$Poles_RT_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

#######################
###### Paroisses ######
#######################

observeEvent(input$Paroisses_Nb_Haut_Rate,{
  req(input$Paroisses_Nb_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Paroisses_Nb_Haut_Rate))[3],
                      input$Paroisses_Nb_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Paroisses_Nb_Bas_Rate,{
  req(input$Paroisses_Nb_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Paroisses_Nb_Bas_Rate))[3],
                      input$Paroisses_Nb_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Paroisses_Compo_Haut_Rate,{
  req(input$Paroisses_Compo_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Paroisses_Compo_Haut_Rate))[3],
                      input$Paroisses_Compo_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Paroisses_Compo_Bas_Rate,{
  req(input$Paroisses_Compo_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Paroisses_Compo_Bas_Rate))[3],
                      input$Paroisses_Compo_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Paroisses_Promo_Haut_Rate,{
  req(input$Paroisses_Promo_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Paroisses_Promo_Haut_Rate))[3],
                      input$Paroisses_Promo_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Paroisses_Promo_Bas_Rate,{
  req(input$Paroisses_Promo_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Paroisses_Promo_Bas_Rate))[3],
                      input$Paroisses_Promo_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Paroisses_Superficie_Haut_Rate,{
  req(input$Paroisses_Superficie_Haut_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Paroisses_Superficie_Haut_Rate))[3],
                      input$Paroisses_Superficie_Haut_Rate,
                      paste(as.character(filtredSeedsHaut()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)

observeEvent(input$Paroisses_Superficie_Bas_Rate,{
  req(input$Paroisses_Superficie_Bas_Rate)
  thisLine <- sprintf("%s, %s, %s, %s",
                      isolate(input$userName),
                      as.character(rlang::expr(input$Paroisses_Superficie_Bas_Rate))[3],
                      input$Paroisses_Superficie_Bas_Rate,
                      paste(as.character(filtredSeedsBas()), collapse = ";"))
  write_lines(x = thisLine, path = "data/results_rating.csv", append = TRUE)
  }
)