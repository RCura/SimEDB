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
                                                            ),
                                                            fluidRow(
                                                              column(6, downloadButton("FP_TypeDeplacements_DL", label = "Télécharger PDF")),
                                                              column(6, downloadButton("FP_TypeDeplacements_Filter_DL", label = "Télécharger PDF")))
                                                   ),
                                                   tabPanel("Déplacements (détail)", 
                                                            fluidRow(
                                                              column(6,plotOutput("FP_DeplacementsDetail")),
                                                              column(6,plotOutput("FP_DeplacementsDetail_Filter"))
                                                            ),
                                                            fluidRow(
                                                              column(6, downloadButton("FP_DeplacementsDetail_DL", label = "Télécharger PDF")),
                                                              column(6, downloadButton("FP_DeplacementsDetail_Filter_DL", label = "Télécharger PDF")))
                                                   ),
                                                   tabPanel("Concentration",
                                                            fluidRow(
                                                              column(6,plotOutput("FP_Concentration")),
                                                              column(6,plotOutput("FP_Concentration_Filter"))
                                                            ),
                                                            fluidRow(
                                                              column(6, downloadButton("FP_Concentration_DL", label = "Télécharger PDF")),
                                                              column(6, downloadButton("FP_Concentration_Filter_DL", label = "Télécharger PDF")))
                                                   ),
                                                   tabPanel("Satisfaction",
                                                            fluidRow(
                                                              column(6,plotOutput("FP_Satisfaction")),
                                                              column(6,plotOutput("FP_Satisfaction_Filter"))
                                                            ),
                                                            fluidRow(
                                                              column(6, downloadButton("FP_Satisfaction_DL", label = "Télécharger PDF")),
                                                              column(6, downloadButton("FP_Satisfaction_Filter_DL", label = "Télécharger PDF")))
                                                   )
                                       )),
                              tabPanel("Agrégats",
                                       tabsetPanel(id = "agregatsPlots",
                                                   tabPanel("Nombre",
                                                            fluidRow(
                                                              column(6,plotOutput("Agregats_Nb")),
                                                              column(6, plotOutput("Agregats_Nb_Filter"))
                                                            ),
                                                            fluidRow(
                                                              column(6, downloadButton("Agregats_Nb_DL", label = "Télécharger PDF")),
                                                              column(6, downloadButton("Agregats_Nb_Filter_DL", label = "Télécharger PDF")))
                                                   ),
                                                   tabPanel("Pôles",
                                                            fluidRow(
                                                              column(6,plotOutput("Agregats_Poles")),
                                                              column(6, plotOutput("Agregats_Poles_Filter"))
                                                            ),
                                                            fluidRow(
                                                              column(6, downloadButton("Agregats_Poles_DL", label = "Télécharger PDF")),
                                                              column(6, downloadButton("Agregats_Poles_Filter_DL", label = "Télécharger PDF")))
                                                   ),
                                                   tabPanel("Communauté",
                                                            fluidRow(
                                                              column(6,plotOutput("Agregats_CA")),
                                                              column(6, plotOutput("Agregats_CA_Filter"))
                                                            ),
                                                            fluidRow(
                                                              column(6, downloadButton("Agregats_CA_DL", label = "Télécharger PDF")),
                                                              column(6, downloadButton("Agregats_CA_Filter_DL", label = "Télécharger PDF")))
                                                   ),
                                                   tabPanel("Hiérarchie",
                                                            fluidRow(
                                                              column(6,plotOutput("Agregats_RT")),
                                                              column(6, plotOutput("Agregats_RT_Filter"))
                                                            ),
                                                            fluidRow(
                                                              column(6, downloadButton("Agregats_RT_DL", label = "Télécharger PDF")),
                                                              column(6, downloadButton("Agregats_RT_Filter_DL", label = "Télécharger PDF")))
                                                   ))),
                                       tabPanel("Seigneurs",
                                                tabsetPanel(id = "seigneursPlots",
                                                            tabPanel("Nombre",
                                                                     fluidRow(
                                                                       column(6,plotOutput("Seigneurs_Nb")),
                                                                       column(6, plotOutput("Seigneurs_Nb_Filter"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(6, downloadButton("Seigneurs_Nb_DL", label = "Télécharger PDF")),
                                                                       column(6, downloadButton("Seigneurs_Nb_Filter_DL", label = "Télécharger PDF")))
                                                            ),
                                                            tabPanel("Chateaux",
                                                                     fluidRow(
                                                                       column(6,plotOutput("Seigneurs_Chateaux")),
                                                                       column(6, plotOutput("Seigneurs_Chateaux_Filter"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(6, downloadButton("Seigneurs_Chateaux_DL", label = "Télécharger PDF")),
                                                                       column(6, downloadButton("Seigneurs_Chateaux_Filter_DL", label = "Télécharger PDF")))
                                                            ),
                                                            tabPanel("Vassaux",
                                                                     fluidRow(
                                                                       column(6,plotOutput("Seigneurs_Vassaux")),
                                                                       column(6, plotOutput("Seigneurs_Vassaux_Filter"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(6, downloadButton("Seigneurs_Vassaux_DL", label = "Télécharger PDF")),
                                                                       column(6, downloadButton("Seigneurs_Vassaux_Filter_DL", label = "Télécharger PDF")))
                                                            ),
                                                            tabPanel("Redevances (global)",
                                                                     fluidRow(
                                                                       column(6,plotOutput("Seigneurs_Redevances")),
                                                                       column(6, plotOutput("Seigneurs_Redevances_Filter"))
                                                                     ),   
                                                                     fluidRow(
                                                                       column(6, downloadButton("Seigneurs_Redevances_DL", label = "Télécharger PDF")),
                                                                       column(6, downloadButton("Seigneurs_Redevances_Filter_DL", label = "Télécharger PDF")))
                                                            ),
                                                            tabPanel("Redevances (détail)",
                                                                     fluidRow(
                                                                       column(6,plotOutput("Seigneurs_Redevances_PS")),
                                                                       column(6, plotOutput("Seigneurs_Redevances_PS_Filter"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(6, downloadButton("Seigneurs_Redevances_PS_DL", label = "Télécharger PDF")),
                                                                       column(6, downloadButton("Seigneurs_Redevances_PS_Filter_DL", label = "Télécharger PDF")))
                                                            ),
                                                            tabPanel("Puissance",
                                                                     fluidRow(
                                                                       column(6,plotOutput("Seigneurs_Puissance")),
                                                                       column(6, plotOutput("Seigneurs_Puissance_Filter"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(6, downloadButton("Seigneurs_Puissance_DL", label = "Télécharger PDF")),
                                                                       column(6, downloadButton("Seigneurs_Puissance_Filter_DL", label = "Télécharger PDF")))
                                                            ),
                                                            tabPanel("Agrégats",
                                                                     fluidRow(
                                                                       column(6,plotOutput("Seigneurs_Agregats")),
                                                                       column(6, plotOutput("Seigneurs_Agregats_Filter"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(6, downloadButton("Seigneurs_Agregats_DL", label = "Télécharger PDF")),
                                                                       column(6, downloadButton("Seigneurs_Agregats_Filter_DL", label = "Télécharger PDF")))
                                                            )
                                                )),
                                       tabPanel("Pôles",
                                                tabsetPanel(id = "polesPlots",
                                                            tabPanel("Nombre",
                                                                     fluidRow(
                                                                       column(6,plotOutput("Poles_Nb")),
                                                                       column(6, plotOutput("Poles_Nb_Filter"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(6, downloadButton("Poles_Nb_DL", label = "Télécharger PDF")),
                                                                       column(6, downloadButton("Poles_Nb_Filter_DL", label = "Télécharger PDF")))
                                                            ),
                                                            tabPanel("Pôles d'agrégats",
                                                                     fluidRow(
                                                                       column(6,plotOutput("Poles_Agregats")),
                                                                       column(6, plotOutput("Poles_Agregats_Filter"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(6, downloadButton("Poles_Agregats_DL", label = "Télécharger PDF")),
                                                                       column(6, downloadButton("Poles_Agregats_Filter_DL", label = "Télécharger PDF")))
                                                            ),
                                                            tabPanel("Attracteurs",
                                                                     fluidRow(
                                                                       column(6,plotOutput("Poles_Compo")),
                                                                       column(6, plotOutput("Poles_Compo_Filter"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(6, downloadButton("Poles_Compo_DL", label = "Télécharger PDF")),
                                                                       column(6, downloadButton("Poles_Compo_Filter_DL", label = "Télécharger PDF")))
                                                            ),
                                                            tabPanel("Attractivité",
                                                                     fluidRow(
                                                                       column(6,plotOutput("Poles_Attrac")),
                                                                       column(6, plotOutput("Poles_Attrac_Filter"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(6, downloadButton("Poles_Attrac_DL", label = "Télécharger PDF")),
                                                                       column(6, downloadButton("Poles_Attrac_Filter_DL", label = "Télécharger PDF")))
                                                            ),
                                                            tabPanel("Hiérarchie",
                                                                     fluidRow(
                                                                       column(6,plotOutput("Poles_RT")),
                                                                       column(6, plotOutput("Poles_RT_Filter"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(6, downloadButton("Poles_RT_DL", label = "Télécharger PDF")),
                                                                       column(6, downloadButton("Poles_RT_Filter_DL", label = "Télécharger PDF")))
                                                            ))),
                                       tabPanel("Paroisses",
                                                tabsetPanel(id = "paroissesPlots",
                                                            tabPanel("Nombre",
                                                                     fluidRow(
                                                                       column(6,plotOutput("Paroisses_Nb")),
                                                                       column(6, plotOutput("Paroisses_Nb_Filter"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(6, downloadButton("Paroisses_Nb_DL", label = "Télécharger PDF")),
                                                                       column(6, downloadButton("Paroisses_Nb_Filter_DL", label = "Télécharger PDF")))
                                                            ),
                                                            tabPanel("Composition (Nb FP/ paroisse)",
                                                                     fluidRow(
                                                                       column(6,plotOutput("Paroisses_Compo")),
                                                                       column(6, plotOutput("Paroisses_Compo_Filter"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(6, downloadButton("Paroisses_Compo_DL", label = "Télécharger PDF")),
                                                                       column(6, downloadButton("Paroisses_Compo_Filter_DL", label = "Télécharger PDF")))
                                                            ),
                                                            tabPanel("Modes de promotion",
                                                                     fluidRow(
                                                                       column(6,plotOutput("Paroisses_Promo")),
                                                                       column(6, plotOutput("Paroisses_Promo_Filter"))
                                                                     ), 
                                                                     fluidRow(
                                                                       column(6, downloadButton("Paroisses_Promo_DL", label = "Télécharger PDF")),
                                                                       column(6, downloadButton("Paroisses_Promo_Filter_DL", label = "Télécharger PDF")))
                                                            ),
                                                            tabPanel("Superficie",
                                                                     fluidRow(
                                                                       column(6,plotOutput("Paroisses_Superficie")),
                                                                       column(6, plotOutput("Paroisses_Superficie_Filter"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(6, downloadButton("Paroisses_Superficie_DL", label = "Télécharger PDF")),
                                                                       column(6, downloadButton("Paroisses_Superficie_Filter_DL", label = "Télécharger PDF")))
                                                            )))
                              ))
                  )