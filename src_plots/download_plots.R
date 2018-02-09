########################
########## FP ##########
########################

output$FP_TypeDeplacements_Haut_DL <- downloadHandler(
  filename = function(){"FP_TypeDeplacements_Haut.pdf"},
  content = function(file){ ggsave(file, plot = FP_TypeDeplacements(filtredHaut$FP), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$FP_TypeDeplacements_Bas_DL <- downloadHandler(
  filename = function(){"FP_TypeDeplacements_Bas.pdf"},
  content = function(file){ ggsave(file, plot = FP_TypeDeplacements(filtredBas$FP), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$FP_DeplacementsDetail_Haut_DL <- downloadHandler(
  filename = function(){"FP_DeplacementsDetail_Haut.pdf"},
  content = function(file){ ggsave(file, plot = FP_DeplacementsDetail(filtredHaut$FP), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$FP_DeplacementsDetail_Bas_DL <- downloadHandler(
  filename = function(){"FP_DeplacementsDetail_Bas.pdf"},
  content = function(file){ ggsave(file, plot = FP_DeplacementsDetail(filtredBas$FP), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$FP_Concentration_Haut_DL <- downloadHandler(
  filename = function(){"FP_Concentration_Haut.pdf"},
  content = function(file){ ggsave(file, plot = FP_Concentration(filtredHaut$results), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$FP_Concentration_Bas_DL <- downloadHandler(
  filename = function(){"FP_Concentration_Bas.pdf"},
  content = function(file){ ggsave(file, plot = FP_Concentration(filtredBas$results), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$FP_Satisfaction_Haut_DL <- downloadHandler(
  filename = function(){"FP_Satisfaction_Haut.pdf"},
  content = function(file){ ggsave(file, plot = FP_Satisfaction(filtredHaut$FP), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$FP_Satisfaction_Bas_DL <- downloadHandler(
  filename = function(){"FP_Satisfaction_Bas.pdf"},
  content = function(file){ ggsave(file, plot = FP_Satisfaction(filtredBas$FP), device ="pdf", width = 20, height = 10, units = "cm") }
)

########################
####### Agrégats #######
########################

output$Agregats_Nb_Haut_DL <- downloadHandler(
  filename = function(){"Agregats_Nb_Haut.pdf"},
  content = function(file){ ggsave(file, plot = Agregats_Nb(filtredHaut$agregats), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Agregats_Nb_Bas_DL <- downloadHandler(
  filename = function(){"Agregats_Nb_Bas.pdf"},
  content = function(file){ ggsave(file, plot = Agregats_Nb(filtredBas$agregats), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Agregats_Poles_Haut_DL <- downloadHandler(
  filename = function(){"Agregats_Poles_Haut.pdf"},
  content = function(file){ ggsave(file, plot = Agregats_Poles(filtredHaut$agregats), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Agregats_Poles_Bas_DL <- downloadHandler(
  filename = function(){"Agregats_Poles_Bas.pdf"},
  content = function(file){ ggsave(file, plot = Agregats_Poles(filtredBas$agregats), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Agregats_Paroisses_Haut_DL <- downloadHandler(
  filename = function(){"Agregats_Paroisses_Haut.pdf"},
  content = function(file){ ggsave(file, plot =  Agregats_Paroisses(agregats_data = filtredHaut$agregats, poles_data = filtredHaut$poles), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Agregats_Paroisses_Bas_DL <- downloadHandler(
  filename = function(){"Agregats_Paroisses_Bas.pdf"},
  content = function(file){ ggsave(file, plot = Agregats_Paroisses(agregats_data = filtredBas$agregats, poles_data = filtredBas$poles), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Agregats_CA_Haut_DL <- downloadHandler(
  filename = function(){"Agregats_CA_Haut.pdf"},
  content = function(file){ ggsave(file, plot = Agregats_CA(filtredHaut$agregats), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Agregats_CA_Bas_DL <- downloadHandler(
  filename = function(){"Agregats_CA_Bas.pdf"},
  content = function(file){ ggsave(file, plot = Agregats_CA(filtredBas$agregats), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Agregats_RT_Haut_DL <- downloadHandler(
  filename = function(){"Agregats_RT_Haut.pdf"},
  content = function(file){ ggsave(file, plot = Agregats_RT(filtredHaut$agregats), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Agregats_RT_Bas_DL <- downloadHandler(
  filename = function(){"Agregats_RT_Bas.pdf"},
  content = function(file){ ggsave(file, plot = Agregats_RT(filtredBas$agregats), device ="pdf", width = 20, height = 10, units = "cm") }
)

#######################
###### Seigneurs ######
#######################

output$Seigneurs_Nb_Haut_DL <- downloadHandler(
  filename = function(){"Seigneurs_Nb_Haut.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Nb(filtredHaut$seigneurs), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Seigneurs_Nb_Bas_DL <- downloadHandler(
  filename = function(){"Seigneurs_Nb_Bas.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Nb(filtredBas$seigneurs), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Seigneurs_Chateaux_Haut_DL <- downloadHandler(
  filename = function(){"Seigneurs_Chateaux_Haut.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Chateaux(filtredHaut$seigneurs), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Seigneurs_Chateaux_Bas_DL <- downloadHandler(
  filename = function(){"Seigneurs_Chateaux_Haut_DL_Haut.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Chateaux(filtredBas$seigneurs), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Seigneurs_Vassaux_Haut_DL <- downloadHandler(
  filename = function(){"Seigneurs_Vassaux_Haut.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Vassaux(filtredHaut$seigneurs), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Seigneurs_Vassaux_Bas_DL <- downloadHandler(
  filename = function(){"Seigneurs_Vassaux_Bas.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Vassaux(filtredBas$seigneurs), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Seigneurs_Redevances_Haut_DL <- downloadHandler(
  filename = function(){"Seigneurs_Redevances_Haut.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Redevances(filtredHaut$seigneurs), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Seigneurs_Redevances_Bas_DL <- downloadHandler(
  filename = function(){"Seigneurs_Redevances_Bas.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Redevances(filtredBas$seigneurs), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Seigneurs_Redevances_PS_Haut_DL <- downloadHandler(
  filename = function(){"Seigneurs_Redevances_PS_Haut.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Redevances_PS(filtredHaut$seigneurs), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Seigneurs_Redevances_PS_Bas_DL <- downloadHandler(
  filename = function(){"Seigneurs_Redevances_PS_Bas.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Redevances_PS(filtredBas$seigneurs), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Seigneurs_Puissance_Haut_DL <- downloadHandler(
  filename = function(){"Seigneurs_Puissance_Haut.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Puissance(filtredHaut$seigneurs), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Seigneurs_Puissance_Bas_DL <- downloadHandler(
  filename = function(){"Seigneurs_Puissance_Bas.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Puissance(filtredBas$seigneurs), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Seigneurs_Agregats_Haut_DL <- downloadHandler(
  filename = function(){"Seigneurs_Agregats_Haut.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Agregats(filtredHaut$seigneurs, filtredHaut$agregats), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Seigneurs_Agregats_Bas_DL <- downloadHandler(
  filename = function(){"Seigneurs_Agregats_Bas.pdf"},
  content = function(file){ ggsave(file, plot = Seigneurs_Agregats(filtredBas$seigneurs, filtredBas$agregats), device ="pdf", width = 20, height = 10, units = "cm") }
)


###################
###### Poles ######
###################

output$Poles_Nb_Haut_DL <- downloadHandler(
  filename = function(){"Poles_Nb_Haut.pdf"},
  content = function(file){ ggsave(file, plot = Poles_Nb(filtredHaut$poles), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Poles_Nb_Bas_DL <- downloadHandler(
  filename = function(){"Poles_Nb_Bas.pdf"},
  content = function(file){ ggsave(file, plot = Poles_Nb(filtredBas$poles), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Poles_Agregats_Haut_DL <- downloadHandler(
  filename = function(){"Poles_Agregats_Haut.pdf"},
  content = function(file){ ggsave(file, plot = Poles_Agregats(filtredHaut$poles), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Poles_Agregats_Bas_DL <- downloadHandler(
  filename = function(){"Poles_Agregats_Bas.pdf"},
  content = function(file){ ggsave(file, plot = Poles_Agregats(filtredBas$poles), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Poles_Compo_Haut_DL <- downloadHandler(
  filename = function(){"Poles_Compo_Haut.pdf"},
  content = function(file){ ggsave(file, plot = Poles_Compo(filtredHaut$poles), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Poles_Compo_Bas_DL <- downloadHandler(
  filename = function(){"Poles_Compo_Bas.pdf"},
  content = function(file){ ggsave(file, plot = Poles_Compo(filtredBas$poles), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Poles_Attrac_Haut_DL <- downloadHandler(
  filename = function(){"Poles_Attrac_Haut.pdf"},
  content = function(file){ ggsave(file, plot = Poles_Attrac(filtredHaut$poles), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Poles_Attrac_Bas_DL <- downloadHandler(
  filename = function(){"Poles_Attrac_Bas.pdf"},
  content = function(file){ ggsave(file, plot = Poles_Attrac(filtredBas$poles), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Poles_RT_Haut_DL <- downloadHandler(
  filename = function(){"Poles_RT_Haut.pdf"},
  content = function(file){ ggsave(file, plot = Poles_RT(filtredHaut$poles), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Poles_RT_Bas_DL <- downloadHandler(
  filename = function(){"Poles_RT_Bas.pdf"},
  content = function(file){ ggsave(file, plot = Poles_RT(filtredBas$poles), device ="pdf", width = 20, height = 10, units = "cm") }
)

#######################
###### Paroisses ######
#######################

output$Paroisses_Nb_Haut_DL <- downloadHandler(
  filename = function(){"Paroisses_Nb_Haut.pdf"},
  content = function(file){ ggsave(file, plot = Paroisses_Nb(filtredHaut$paroisses), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Paroisses_Nb_Bas_DL <- downloadHandler(
  filename = function(){"Paroisses_Nb_Bas.pdf"},
  content = function(file){ ggsave(file, plot = Paroisses_Nb(filtredBas$paroisses), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Paroisses_Compo_Haut_DL <- downloadHandler(
  filename = function(){"Paroisses_Compo_Haut.pdf"},
  content = function(file){ ggsave(file, plot = Paroisses_Compo(filtredHaut$paroisses), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Paroisses_Compo_Bas_DL <- downloadHandler(
  filename = function(){"Paroisses_Compo_Bas.pdf"},
  content = function(file){ ggsave(file, plot = Paroisses_Compo(filtredBas$paroisses), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Paroisses_Promo_Haut_DL <- downloadHandler(
  filename = function(){"Paroisses_Promo_Haut.pdf"},
  content = function(file){ ggsave(file, plot = Paroisses_Promo(filtredHaut$paroisses), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Paroisses_Promo_Bas_DL <- downloadHandler(
  filename = function(){"Paroisses_Promo_Bas.pdf"},
  content = function(file){ ggsave(file, plot = Paroisses_Promo(filtredBas$paroisses), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Paroisses_Superficie_Haut_DL <- downloadHandler(
  filename = function(){"Paroisses_Superficie_Haut.pdf"},
  content = function(file){ ggsave(file, plot = Paroisses_Superficie(filtredHaut$paroisses), device ="pdf", width = 20, height = 10, units = "cm") }
)

output$Paroisses_Superficie_Bas_DL <- downloadHandler(
  filename = function(){"Paroisses_Superficie_Bas.pdf"},
  content = function(file){ ggsave(file, plot = Paroisses_Superficie(filtredBas$paroisses), device ="pdf", width = 20, height = 10, units = "cm") }
)