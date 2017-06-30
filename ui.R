library(shiny)

shinyUI(fluidPage(
  titlePanel("SimVADB"),
  sidebarLayout(sidebarPanel(width = 4,fluid = TRUE,
                                  textOutput("dataVolume",inline =  TRUE),
                                   #parcoordsOutput("paramParCoords", width = "100%", height = "300px"),
                                   #dataTableOutput("paramLegend"),
                                   plotOutput("simNames"),
                                   plotOutput("resultsPlot")
                                   ),
                      mainPanel(
                        # column(10,
                        #   
                        # ),
                        #column(10,
                        
                        tabsetPanel(id = "detailPlots",type = "pills",
                                    tabPanel("Objectifs généraux",
                                             fluidRow(formattableOutput("targetsTable", width = "95%")),
                                             fluidRow(formattableOutput("selectionTable", width = "95%"))
                                    ),
                                    tabPanel("Foyers Paysans",
                                             tabsetPanel(id = "FPPlots", position = "left",
                                                         tabPanel("Déplacements", 
                                                                  fluidRow(
                                                                    column(10, plotOutput("FP_TypeDeplacements")),
                                                                    column(2, downloadButton("FP_TypeDeplacements_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("FP_TypeDeplacements_filter")),
                                                                    column(2,downloadButton("FP_TypeDeplacements_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         ),
                                                         tabPanel("Déplacements (détail)", 
                                                                  fluidRow(
                                                                    column(10, plotOutput("FP_DeplacementsDetail")),
                                                                    column(2, downloadButton("FP_DeplacementsDetail_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("FP_DeplacementsDetail_Filter")),
                                                                    column(2,downloadButton("FP_DeplacementsDetail_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         ),
                                                         tabPanel("Concentration", 
                                                                  fluidRow(
                                                                    column(10, plotOutput("FP_Concentration")),
                                                                    column(2, downloadButton("FP_Concentration_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("FP_Concentration_Filter")),
                                                                    column(2,downloadButton("FP_Concentration_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         ),
                                                         tabPanel("Satisfaction", 
                                                                  fluidRow(
                                                                    column(10, plotOutput("FP_Satisfaction")),
                                                                    column(2, downloadButton("FP_Satisfaction_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("FP_Satisfaction_Filter")),
                                                                    column(2,downloadButton("FP_Satisfaction_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         )
                                             )),
                                    tabPanel("Agrégats",
                                             tabsetPanel(id = "agregatsPlots",
                                                         tabPanel("Nombre", 
                                                                  fluidRow(
                                                                    column(10, plotOutput("Agregats_Nb")),
                                                                    column(2, downloadButton("Agregats_Nb_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("Agregats_Nb_Filter")),
                                                                    column(2,downloadButton("Agregats_Nb_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         ),
                                                         tabPanel("Pôles", 
                                                                  fluidRow(
                                                                    column(10, plotOutput("Agregats_Poles")),
                                                                    column(2, downloadButton("Agregats_Poles_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("Agregats_Poles_Filter")),
                                                                    column(2,downloadButton("Agregats_Poles_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         ),
                                                         tabPanel("Communauté", 
                                                                  fluidRow(
                                                                    column(10, plotOutput("Agregats_CA")),
                                                                    column(2, downloadButton("Agregats_CA_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("Agregats_CA_Filter")),
                                                                    column(2,downloadButton("Agregats_CA_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         ),
                                                         tabPanel("Hiérarchie", 
                                                                  fluidRow(
                                                                    column(10, plotOutput("Agregats_RT")),
                                                                    column(2, downloadButton("Agregats_RT_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("Agregats_RT_Filter")),
                                                                    column(2,downloadButton("Agregats_RT_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         ))),
                                    tabPanel("Seigneurs",
                                             tabsetPanel(id = "seigneursPlots",
                                                         tabPanel("Nombre", 
                                                                  fluidRow(
                                                                    column(10, plotOutput("Seigneurs_Nb")),
                                                                    column(2, downloadButton("Seigneurs_Nb_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("Seigneurs_Nb_Filter")),
                                                                    column(2,downloadButton("Seigneurs_Nb_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         ),
                                                         tabPanel("Chateaux", 
                                                                  fluidRow(
                                                                    column(10, plotOutput("Seigneurs_Chateaux")),
                                                                    column(2, downloadButton("Seigneurs_Chateaux_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("Seigneurs_Chateaux_Filter")),
                                                                    column(2,downloadButton("Seigneurs_Chateaux_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         ),
                                                         tabPanel("Vassaux", 
                                                                  fluidRow(
                                                                    column(10, plotOutput("Seigneurs_Vassaux")),
                                                                    column(2, downloadButton("Seigneurs_Vassaux_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("Seigneurs_Vassaux_Filter")),
                                                                    column(2,downloadButton("Seigneurs_Vassaux_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         ),
                                                         tabPanel("Redevances (global)",
                                                                  fluidRow(
                                                                    column(10, plotOutput("Seigneurs_Redevances")),
                                                                    column(2, downloadButton("Seigneurs_Redevances_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("Seigneurs_Redevances_Filter")),
                                                                    column(2,downloadButton("Seigneurs_Redevances_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         ),
                                                         tabPanel("Redevances (détail)",
                                                                  fluidRow(
                                                                    column(10, plotOutput("Seigneurs_Redevances_PS")),
                                                                    column(2, downloadButton("Seigneurs_Redevances_PS_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("Seigneurs_Redevances_PS_Filter")),
                                                                    column(2,downloadButton("Seigneurs_Redevances_PS_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         ),
                                                         tabPanel("Puissance",
                                                                  fluidRow(
                                                                    column(10, plotOutput("Seigneurs_Puissance")),
                                                                    column(2, downloadButton("Seigneurs_Puissance_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("Seigneurs_Puissance_Filter")),
                                                                    column(2,downloadButton("Seigneurs_Puissance_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         ),
                                                         tabPanel("Agrégats",
                                                                  fluidRow(
                                                                    column(10, plotOutput("Seigneurs_Agregats")),
                                                                    column(2, downloadButton("Seigneurs_Agregats_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("Seigneurs_Agregats_Filter")),
                                                                    column(2,downloadButton("Seigneurs_Agregats_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         )
                                             )),
                                    tabPanel("Pôles",
                                             tabsetPanel(id = "polesPlots", position = "left",
                                                         tabPanel("Nombre", 
                                                                  fluidRow(
                                                                    column(10, plotOutput("Poles_Nb")),
                                                                    column(2, downloadButton("Poles_Nb_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("Poles_Nb_Filter")),
                                                                    column(2,downloadButton("Poles_Nb_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         ),
                                                         tabPanel("Pôles d'agrégats", 
                                                                  fluidRow(
                                                                    column(10, plotOutput("Poles_Agregats")),
                                                                    column(2, downloadButton("Poles_Agregats_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("Poles_Agregats_Filter")),
                                                                    column(2,downloadButton("Poles_Agregats_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         ),
                                                         tabPanel("Attracteurs", 
                                                                  fluidRow(
                                                                    column(10, plotOutput("Poles_Compo")),
                                                                    column(2, downloadButton("Poles_Compo_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("Poles_Compo_Filter")),
                                                                    column(2,downloadButton("Poles_Compo_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         ),
                                                         tabPanel("Attractivité", 
                                                                  fluidRow(
                                                                    column(10, plotOutput("Poles_Attrac")),
                                                                    column(2, downloadButton("Poles_Attrac_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("Poles_Attrac_Filter")),
                                                                    column(2,downloadButton("Poles_Attrac_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         ),
                                                         tabPanel("Hiérarchie", 
                                                                  fluidRow(
                                                                    column(10, plotOutput("Poles_RT")),
                                                                    column(2, downloadButton("Poles_RT_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("Poles_RT_Filter")),
                                                                    column(2,downloadButton("Poles_RT_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         )
                                             )),
                                    tabPanel("Paroisses",
                                             tabsetPanel(id = "paroissesPlots", position = "left",
                                                         tabPanel("Nombre", 
                                                                  fluidRow(
                                                                    column(10, plotOutput("Paroisses_Nb")),
                                                                    column(2, downloadButton("Paroisses_Nb_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("Paroisses_Nb_Filter")),
                                                                    column(2,downloadButton("Paroisses_Nb_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         ),
                                                         tabPanel("Composition (Nb FP/ paroisse)",
                                                                  fluidRow(
                                                                    column(10, plotOutput("Paroisses_Compo")),
                                                                    column(2, downloadButton("Paroisses_Compo_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("Paroisses_Compo_Filter")),
                                                                    column(2,downloadButton("Paroisses_Compo_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         ),
                                                         tabPanel("Modes de promotion",
                                                                  fluidRow(
                                                                    column(10, plotOutput("Paroisses_Promo")),
                                                                    column(2, downloadButton("Paroisses_Promo_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("Paroisses_Promo_Filter")),
                                                                    column(2,downloadButton("Paroisses_Promo_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         ),
                                                         tabPanel("Superficie", 
                                                                  fluidRow(
                                                                    column(10, plotOutput("Paroisses_Superficie")),
                                                                    column(2, downloadButton("Paroisses_Superficie_DL", label = "Télécharger PDF"))
                                                                  ),
                                                                  fluidRow(
                                                                    column(10,plotOutput("Paroisses_Superficie_Filter")),
                                                                    column(2,downloadButton("Paroisses_Superficie_Filter_DL", label = "Télécharger PDF"))  
                                                                  )
                                                         )
                                             ))
                        ))
  )
)
)