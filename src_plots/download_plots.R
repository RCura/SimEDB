########################
########## FP ##########
########################

output$FP_TypeDeplacements_DL <- downloadHandler(
  filename = function(){"FP_TypeDeplacements.pdf"},
  content = function(file){ ggsave(file, plot = FP_TypeDeplacements(sim$FP), device = "pdf") }
)

output$FP_TypeDeplacements_Filter_DL <- downloadHandler(
  filename = function(){"FP_TypeDeplacements_Filter.pdf"},
  content = function(file){ ggsave(file, plot = FP_TypeDeplacements(filtred$FP), device = "pdf") }
)

output$FP_DeplacementsDetail_DL <- downloadHandler(
  filename = function(){"FP_DeplacementsDetail.pdf"},
  content = function(file){ ggsave(file, plot = FP_DeplacementsDetail(sim$FP), device = "pdf") }
)

output$FP_DeplacementsDetail_Filter_DL <- downloadHandler(
  filename = function(){"FP_DeplacementsDetail_Filter.pdf"},
  content = function(file){ ggsave(file, plot = FP_DeplacementsDetail(filtred$FP), device = "pdf") }
)

output$FP_Concentration_DL <- downloadHandler(
  filename = function(){"FP_Concentration.pdf"},
  content = function(file){ ggsave(file, plot = FP_Concentration(sim$results), device = "pdf") }
)

output$FP_Concentration_Filter_DL <- downloadHandler(
  filename = function(){"FP_Concentration_Filter.pdf"},
  content = function(file){ ggsave(file, plot = FP_Concentration(filtred$results), device = "pdf") }
)

output$FP_Satisfaction_DL <- downloadHandler(
  filename = function(){"FP_Satisfaction.pdf"},
  content = function(file){ ggsave(file, plot = FP_Satisfaction(sim$FP), device = "pdf") }
)

output$FP_Satisfaction_Filter_DL <- downloadHandler(
  filename = function(){"FP_Satisfaction_Filter.pdf"},
  content = function(file){ ggsave(file, plot = FP_Satisfaction(filtred$FP), device = "pdf") }
)

########################
####### Agrégats #######
########################

output$Agregats_Nb_DL <- downloadHandler(
  filename = function(){"Agregats_Nb.pdf"},
  content = function(file){ ggsave(file, plot = Agregats_Nb(sim$agregats), device = "pdf") }
)

output$Agregats_Nb_Filter_DL <- downloadHandler(
  filename = function(){"Agregats_Nb_Filter.pdf"},
  content = function(file){ ggsave(file, plot = Agregats_Nb(filtred$agregats), device = "pdf") }
)

output$Agregats_Poles_DL <- downloadHandler(
  filename = function(){"Agregats_Poles.pdf"},
  content = function(file){ ggsave(file, plot = Agregats_Poles(sim$agregats), device = "pdf") }
)

output$Agregats_Poles_Filter_DL <- downloadHandler(
  filename = function(){"Agregats_Poles_Filter.pdf"},
  content = function(file){ ggsave(file, plot = Agregats_Poles(filtred$agregats), device = "pdf") }
)

output$Agregats_Paroisses_DL <- downloadHandler(
  filename = function(){"Agregats_Paroisses.pdf"},
  content = function(file){ ggsave(file, plot =  Agregats_Paroisses(agregats_data = sim$agregats, poles_data = sim$poles), device = "pdf") }
)

output$Agregats_Paroisses_Filter_DL <- downloadHandler(
  filename = function(){"Agregats_Paroisses_Filter.pdf"},
  content = function(file){ ggsave(file, plot = Agregats_Paroisses(agregats_data = filtred$agregats, poles_data = filtred$poles), device = "pdf") }
)

output$Agregats_CA_DL <- downloadHandler(
  filename = function(){"Agregats_CA.pdf"},
  content = function(file){ ggsave(file, plot = Agregats_CA(sim$agregats), device = "pdf") }
)

output$Agregats_CA_Filter_DL <- downloadHandler(
  filename = function(){"Agregats_CA_Filter.pdf"},
  content = function(file){ ggsave(file, plot = Agregats_CA(filtred$agregats), device = "pdf") }
)

output$Agregats_RT_DL <- downloadHandler(
  filename = function(){"Agregats_RT.pdf"},
  content = function(file){ ggsave(file, plot = Agregats_RT(sim$agregats), device = "pdf") }
)

output$Agregats_RT_Filter_DL <- downloadHandler(
  filename = function(){"Agregats_RT_Filter.pdf"},
  content = function(file){ ggsave(file, plot = Agregats_RT(filtred$agregats), device = "pdf") }
)

#######################
###### Seigneurs ######
#######################

output$Seigneurs_Nb_DL <- downloadHandler(
  filename = function(){"Seigneurs_Nb.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Nb(sim$seigneurs), device = "pdf") }
)

output$Seigneurs_Nb_Filter_DL <- downloadHandler(
  filename = function(){"Seigneurs_Nb_Filter.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Nb(filtred$seigneurs), device = "pdf") }
)

output$Seigneurs_Chateaux_DL <- downloadHandler(
  filename = function(){"Seigneurs_Chateaux.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Chateaux(sim$seigneurs), device = "pdf") }
)

output$Seigneurs_Chateaux_Filter_DL <- downloadHandler(
  filename = function(){"Seigneurs_Chateaux_DL.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Chateaux(filtred$seigneurs), device = "pdf") }
)

output$Seigneurs_Vassaux_DL <- downloadHandler(
  filename = function(){"Seigneurs_Vassaux.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Vassaux(sim$seigneurs), device = "pdf") }
)

output$Seigneurs_Vassaux_Filter_DL <- downloadHandler(
  filename = function(){"Seigneurs_Vassaux_Filter.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Vassaux(filtred$seigneurs), device = "pdf") }
)

output$Seigneurs_Redevances_DL <- downloadHandler(
  filename = function(){"Seigneurs_Redevances.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Redevances(sim$seigneurs), device = "pdf") }
)

output$Seigneurs_Redevances_Filter_DL <- downloadHandler(
  filename = function(){"Seigneurs_Redevances_Filter.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Redevances(filtred$seigneurs), device = "pdf") }
)

output$Seigneurs_Redevances_PS_DL <- downloadHandler(
  filename = function(){"Seigneurs_Redevances_PS.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Redevances_PS(sim$seigneurs), device = "pdf") }
)

output$Seigneurs_Redevances_PS_Filter_DL <- downloadHandler(
  filename = function(){"Seigneurs_Redevances_PS_Filter.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Redevances_PS(filtred$seigneurs), device = "pdf") }
)

output$Seigneurs_Puissance_DL <- downloadHandler(
  filename = function(){"Seigneurs_Puissance.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Puissance(sim$seigneurs), device = "pdf") }
)

output$Seigneurs_Puissance_Filter_DL <- downloadHandler(
  filename = function(){"Seigneurs_Puissance_Filter.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Puissance(filtred$seigneurs), device = "pdf") }
)

output$Seigneurs_Agregats_DL <- downloadHandler(
  filename = function(){"Seigneurs_Agregats.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Agregats(sim$seigneurs, sim$agregats), device = "pdf") }
)

output$Seigneurs_Agregats_Filter_DL <- downloadHandler(
  filename = function(){"Seigneurs_Agregats_Filter.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Agregats(filtred$seigneurs, filtred$agregats), device = "pdf") }
)


###################
###### Poles ######
###################

output$Poles_Nb_DL <- downloadHandler(
  filename = function(){"Poles_Nb.pdf"},
  content = function(file){ ggsave(file, plot = Poles_Nb(sim$poles), device = "pdf") }
)

output$Poles_Nb_Filter_DL <- downloadHandler(
  filename = function(){"Poles_Nb_Filter.pdf"},
  content = function(file){ ggsave(file, plot = Poles_Nb(filtred$poles), device = "pdf") }
)

output$Poles_Agregats_DL <- downloadHandler(
  filename = function(){"Poles_Agregats.pdf"},
  content = function(file){ ggsave(file, plot = Poles_Agregats(sim$poles), device = "pdf") }
)

output$Poles_Agregats_Filter_DL <- downloadHandler(
  filename = function(){"Poles_Agregats_Filter.pdf"},
  content = function(file){ ggsave(file, plot = Poles_Agregats(filtred$poles), device = "pdf") }
)

output$Poles_Compo_DL <- downloadHandler(
  filename = function(){"Poles_Compo.pdf"},
  content = function(file){ ggsave(file, plot = Poles_Compo(sim$poles), device = "pdf") }
)

output$Poles_Compo_Filter_DL <- downloadHandler(
  filename = function(){"Poles_Compo_Filter.pdf"},
  content = function(file){ ggsave(file, plot = Poles_Compo(filtred$poles), device = "pdf") }
)

output$Poles_Attrac_DL <- downloadHandler(
  filename = function(){"Poles_Attrac.pdf"},
  content = function(file){ ggsave(file, plot = Poles_Attrac(sim$poles), device = "pdf") }
)

output$Poles_Attrac_Filter_DL <- downloadHandler(
  filename = function(){"Poles_Attrac_Filter.pdf"},
  content = function(file){ ggsave(file, plot = Poles_Attrac(filtred$poles), device = "pdf") }
)

output$Poles_RT_DL <- downloadHandler(
  filename = function(){"Poles_RT.pdf"},
  content = function(file){ ggsave(file, plot = Poles_RT(sim$poles), device = "pdf") }
)

output$Poles_RT_Filter_DL <- downloadHandler(
  filename = function(){"Poles_RT_Filter.pdf"},
  content = function(file){ ggsave(file, plot = Poles_RT(filtred$poles), device = "pdf") }
)

#######################
###### Paroisses ######
#######################

output$Paroisses_Nb_DL <- downloadHandler(
  filename = function(){"Paroisses_Nb.pdf"},
  content = function(file){ ggsave(file, plot = Paroisses_Nb(sim$paroisses), device = "pdf") }
)

output$Paroisses_Nb_Filter_DL <- downloadHandler(
  filename = function(){"Paroisses_Nb_Filter.pdf"},
  content = function(file){ ggsave(file, plot = Paroisses_Nb(filtred$paroisses), device = "pdf") }
)

output$Paroisses_Compo_DL <- downloadHandler(
  filename = function(){"Paroisses_Compo.pdf"},
  content = function(file){ ggsave(file, plot = Paroisses_Compo(sim$paroisses), device = "pdf") }
)

output$Paroisses_Compo_Filter_DL <- downloadHandler(
  filename = function(){"Paroisses_Compo_Filter.pdf"},
  content = function(file){ ggsave(file, plot = Paroisses_Compo(filtred$paroisses), device = "pdf") }
)

output$Paroisses_Promo_DL <- downloadHandler(
  filename = function(){"Paroisses_Promo.pdf"},
  content = function(file){ ggsave(file, plot = Paroisses_Promo(sim$paroisses), device = "pdf") }
)

output$Paroisses_Promo_Filter_DL <- downloadHandler(
  filename = function(){"Paroisses_Promo_Filter.pdf"},
  content = function(file){ ggsave(file, plot = Paroisses_Promo(filtred$paroisses), device = "pdf") }
)

output$Paroisses_Superficie_DL <- downloadHandler(
  filename = function(){"Paroisses_Superficie.pdf"},
  content = function(file){ ggsave(file, plot = Paroisses_Superficie(sim$paroisses), device = "pdf") }
)

output$Paroisses_Superficie_Filter_DL <- downloadHandler(
  filename = function(){"Paroisses_Superficie_Filter.pdf"},
  content = function(file){ ggsave(file, plot = Paroisses_Superficie(filtred$paroisses), device = "pdf") }
)