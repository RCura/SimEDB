library(shiny)

shinyUI(fluidPage(theme = shinytheme("cerulean"),
  fluidRow(titlePanel("SimVADB")),             
  textOutput("dataVolume",inline =  TRUE),
  
  fluidRow(
    parcoordsOutput("paramParCoords", width = "100%", height = "300px")
  ),
  fluidRow(dataTableOutput("paramLegend"), height = "300px"),
  
  tabsetPanel(id = "detailPlots",type = "pills",
              tabPanel("Objectifs généraux",
                       column(6, formattableOutput("targetsTable")),
                       column(6, formattableOutput("selectionTable"))
                       ),
              tabPanel("Foyers Paysans",
                       tabsetPanel(id = "FPPlots", position = "left",
                                   tabPanel("Déplacements", 
                                            fluidRow(
                                              column(6,plotOutput("FP_TypeDeplacements")),
                                              column(6,plotOutput("FP_TypeDeplacements_filter"))
                                            )),
                                   tabPanel("Déplacements (détail)", 
                                            fluidRow(
                                              column(6,plotOutput("FP_DeplacementsDetail")),
                                              column(6,plotOutput("FP_DeplacementsDetail_Filter"))
                                            )),
                                   tabPanel("Concentration",
                                            fluidRow(
                                              column(6,plotOutput("FP_Concentration")),
                                              column(6,plotOutput("FP_Concentration_Filter"))
                                            )),
                                   tabPanel("Satisfaction",
                                            fluidRow(
                                              column(6,plotOutput("FP_Satisfaction")),
                                              column(6,plotOutput("FP_Satisfaction_Filter"))
                                            ))
                       )),
              tabPanel("Agrégats",
                       tabsetPanel(id = "agregatsPlots",
                                   tabPanel("Nombre",
                                            fluidRow(
                                              column(6,plotOutput("Agregats_Nb")),
                                              column(6, plotOutput("Agregats_Nb_Filter"))
                                              )),
                                   tabPanel("Pôles",
                                            fluidRow(
                                              column(6,plotOutput("Agregats_Poles")),
                                              column(6, plotOutput("Agregats_Poles_Filter"))
                                            )),
                                   tabPanel("Communauté",
                                            fluidRow(
                                              column(6,plotOutput("Agregats_CA")),
                                              column(6, plotOutput("Agregats_CA_Filter"))
                                            )),
                                    tabPanel("Hiérarchie",
                                             fluidRow(
                                               column(6,plotOutput("Agregats_RT")),
                                               column(6, plotOutput("Agregats_RT_Filter"))
                                              ))
                                   )),
              tabPanel("Seigneurs",
                       tabsetPanel(id = "seigneursPlots",
                                   tabPanel("Nombre",
                                            fluidRow(
                                              column(6,plotOutput("Seigneurs_Nb")),
                                              column(6, plotOutput("Seigneurs_Nb_Filter"))
                                            )),
                                   tabPanel("Chateaux",
                                            fluidRow(
                                              column(6,plotOutput("Seigneurs_Chateaux")),
                                              column(6, plotOutput("Seigneurs_Chateaux_Filter"))
                                            )),
                                   tabPanel("Vassaux",
                                            fluidRow(
                                              column(6,plotOutput("Seigneurs_Vassaux")),
                                              column(6, plotOutput("Seigneurs_Vassaux_Filter"))
                                            )),
                                   tabPanel("Redevances (global)",
                                            fluidRow(
                                              column(6,plotOutput("Seigneurs_Redevances")),
                                              column(6, plotOutput("Seigneurs_Redevances_Filter"))
                                            )),
                                   tabPanel("Redevances (détail)",
                                            fluidRow(
                                              column(6,plotOutput("Seigneurs_Redevances_PS")),
                                              column(6, plotOutput("Seigneurs_Redevances_PS_Filter"))
                                            )),
                                   tabPanel("Puissance",
                                            fluidRow(
                                              column(6,plotOutput("Seigneurs_Puissance")),
                                              column(6, plotOutput("Seigneurs_Puissance_Filter"))
                                            )),
                                   tabPanel("Agrégats",
                                            fluidRow(
                                              column(6,plotOutput("Seigneurs_Agregats")),
                                              column(6, plotOutput("Seigneurs_Agregats_Filter"))
                                            ))
                       )),
              tabPanel("Pôles",
                       tabsetPanel(id = "polesPlots",
                                   tabPanel("Nombre",
                                            fluidRow(
                                              column(6,plotOutput("Poles_Nb")),
                                              column(6, plotOutput("Poles_Nb_Filter"))
                                            )),
                                   tabPanel("Pôles d'agrégats",
                                            fluidRow(
                                              column(6,plotOutput("Poles_Agregats")),
                                              column(6, plotOutput("Poles_Agregats_Filter"))
                                            )),
                                   tabPanel("Attracteurs",
                                            fluidRow(
                                              column(6,plotOutput("Poles_Compo")),
                                              column(6, plotOutput("Poles_Compo_Filter"))
                                            )),
                                   tabPanel("Attractivité",
                                            fluidRow(
                                              column(6,plotOutput("Poles_Attrac")),
                                              column(6, plotOutput("Poles_Attrac_Filter"))
                                            )),                                   
                                   tabPanel("Hiérarchie",
                                            fluidRow(
                                              column(6,plotOutput("Poles_RT")),
                                              column(6, plotOutput("Poles_RT_Filter"))
                                            ))
                       )),
              tabPanel("Paroisses",
                       tabsetPanel(id = "paroissesPlots",
                                   tabPanel("Nombre",
                                            fluidRow(
                                              column(6,plotOutput("Paroisses_Nb")),
                                              column(6, plotOutput("Paroisses_Nb_Filter"))
                                            )),
                                   tabPanel("Composition (Nb FP/ paroisse)",
                                            fluidRow(
                                              column(6,plotOutput("Paroisses_Compo")),
                                              column(6, plotOutput("Paroisses_Compo_Filter"))
                                            )),
                                   tabPanel("Modes de promotion",
                                            fluidRow(
                                              column(6,plotOutput("Paroisses_Promo")),
                                              column(6, plotOutput("Paroisses_Promo_Filter"))
                                            )),
                                   tabPanel("Superficie",
                                            fluidRow(
                                              column(6,plotOutput("Paroisses_Superficie")),
                                              column(6, plotOutput("Paroisses_Superficie_Filter"))
                                            ))
                       ))
              )
))
