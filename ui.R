library(shiny)

shinyUI(fluidPage(
  titlePanel("SimVADB"),
  sidebarLayout(sidebarPanel(width = 4,fluid = TRUE,
                             textInput("userName", "Votre nom",  value = "Robin"),
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
                         tabsetPanel(id = "FPPlots", 
                                     tabPanel("Déplacements", 
                                              fluidRow(
                                                column(10, plotOutput("FP_TypeDeplacements")),
                                                column(2,
                                                       fluidRow(downloadButton("FP_TypeDeplacements_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "FP_TypeDeplacements_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                     ),
                                     fluidRow(
                                       column(10,plotOutput("FP_TypeDeplacements_filter")),
                                       column(2,
                                              fluidRow(downloadButton("FP_TypeDeplacements_Filter_DL", label = "")),
                                              fluidRow(ratingInput(inputId = "FP_TypeDeplacements_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                     )
                         ),
                         tabPanel("Déplacements (détail)", 
                                  fluidRow(
                                    column(10, plotOutput("FP_DeplacementsDetail")),
                                    column(2, fluidRow(downloadButton("FP_DeplacementsDetail_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "FP_DeplacementsDetail_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("FP_DeplacementsDetail_Filter")),
                                    column(2, fluidRow(downloadButton("FP_DeplacementsDetail_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "FP_DeplacementsDetail_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         ),
                         tabPanel("Concentration", 
                                  fluidRow(
                                    column(10, plotOutput("FP_Concentration")),
                                    column(2, fluidRow(downloadButton("FP_Concentration_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "FP_Concentration_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("FP_Concentration_Filter")),
                                    column(2, fluidRow(downloadButton("FP_Concentration_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "FP_Concentration_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         ),
                         tabPanel("Satisfaction", 
                                  fluidRow(
                                    column(10, plotOutput("FP_Satisfaction")),
                                    column(2, fluidRow(downloadButton("FP_Satisfaction_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "FP_Satisfaction_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("FP_Satisfaction_Filter")),
                                    column(2, fluidRow(downloadButton("FP_Satisfaction_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "FP_Satisfaction_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         )
                )),
    tabPanel("Agrégats",
             tabsetPanel(id = "agregatsPlots",
                         tabPanel("Nombre", 
                                  fluidRow(
                                    column(10, plotOutput("Agregats_Nb")),
                                    column(2, fluidRow(downloadButton("Agregats_Nb_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Agregats_Nb_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("Agregats_Nb_Filter")),
                                    column(2, fluidRow(downloadButton("Agregats_Nb_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Agregats_Nb_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         ),
                         tabPanel("Pôles", 
                                  fluidRow(
                                    column(10, plotOutput("Agregats_Poles")),
                                    column(2, fluidRow(downloadButton("Agregats_Poles_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Agregats_Poles_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("Agregats_Poles_Filter")),
                                    column(2, fluidRow(downloadButton("Agregats_Poles_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Agregats_Poles_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         ),
                         tabPanel("Communauté", 
                                  fluidRow(
                                    column(10, plotOutput("Agregats_CA")),
                                    column(2, fluidRow(downloadButton("Agregats_CA_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Agregats_CA_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("Agregats_CA_Filter")),
                                    column(2, fluidRow(downloadButton("Agregats_CA_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Agregats_CA_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         ),
                         tabPanel("Hiérarchie", 
                                  fluidRow(
                                    column(10, plotOutput("Agregats_RT")),
                                    column(2, fluidRow(downloadButton("Agregats_RT_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Agregats_RT_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("Agregats_RT_Filter")),
                                    column(2, fluidRow(downloadButton("Agregats_RT_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Agregats_RT_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         ))),
    tabPanel("Seigneurs",
             tabsetPanel(id = "seigneursPlots",
                         tabPanel("Nombre", 
                                  fluidRow(
                                    column(10, plotOutput("Seigneurs_Nb")),
                                    column(2, fluidRow(downloadButton("Seigneurs_Nb_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Seigneurs_Nb_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("Seigneurs_Nb_Filter")),
                                    column(2, fluidRow(downloadButton("Seigneurs_Nb_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Seigneurs_Nb_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         ),
                         tabPanel("Chateaux", 
                                  fluidRow(
                                    column(10, plotOutput("Seigneurs_Chateaux")),
                                    column(2, fluidRow(downloadButton("Seigneurs_Chateaux_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Seigneurs_Chateaux_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("Seigneurs_Chateaux_Filter")),
                                    column(2, fluidRow(downloadButton("Seigneurs_Chateaux_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Seigneurs_Chateaux_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         ),
                         tabPanel("Vassaux", 
                                  fluidRow(
                                    column(10, plotOutput("Seigneurs_Vassaux")),
                                    column(2, fluidRow(downloadButton("Seigneurs_Vassaux_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Seigneurs_Vassaux_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("Seigneurs_Vassaux_Filter")),
                                    column(2, fluidRow(downloadButton("Seigneurs_Vassaux_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Seigneurs_Vassaux_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         ),
                         tabPanel("Redevances (global)",
                                  fluidRow(
                                    column(10, plotOutput("Seigneurs_Redevances")),
                                    column(2, fluidRow(downloadButton("Seigneurs_Redevances_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Seigneurs_Redevances_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("Seigneurs_Redevances_Filter")),
                                    column(2, fluidRow(downloadButton("Seigneurs_Redevances_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Seigneurs_Redevances_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         ),
                         tabPanel("Redevances (détail)",
                                  fluidRow(
                                    column(10, plotOutput("Seigneurs_Redevances_PS")),
                                    column(2, fluidRow(downloadButton("Seigneurs_Redevances_PS_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Seigneurs_Redevances_PS_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("Seigneurs_Redevances_PS_Filter")),
                                    column(2, fluidRow(downloadButton("Seigneurs_Redevances_PS_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Seigneurs_Redevances_PS_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         ),
                         tabPanel("Puissance",
                                  fluidRow(
                                    column(10, plotOutput("Seigneurs_Puissance")),
                                    column(2, fluidRow(downloadButton("Seigneurs_Puissance_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Seigneurs_Puissance_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("Seigneurs_Puissance_Filter")),
                                    column(2, fluidRow(downloadButton("Seigneurs_Puissance_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Seigneurs_Puissance_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         ),
                         tabPanel("Agrégats",
                                  fluidRow(
                                    column(10, plotOutput("Seigneurs_Agregats")),
                                    column(2, fluidRow(downloadButton("Seigneurs_Agregats_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Seigneurs_Agregats_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("Seigneurs_Agregats_Filter")),
                                    column(2, fluidRow(downloadButton("Seigneurs_Agregats_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Seigneurs_Agregats_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         )
             )),
    tabPanel("Pôles",
             tabsetPanel(id = "polesPlots", 
                         tabPanel("Nombre", 
                                  fluidRow(
                                    column(10, plotOutput("Poles_Nb")),
                                    column(2, fluidRow(downloadButton("Poles_Nb_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Poles_Nb_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("Poles_Nb_Filter")),
                                    column(2, fluidRow(downloadButton("Poles_Nb_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Poles_Nb_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         ),
                         tabPanel("Pôles d'agrégats", 
                                  fluidRow(
                                    column(10, plotOutput("Poles_Agregats")),
                                    column(2, fluidRow(downloadButton("Poles_Agregats_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Poles_Agregats_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("Poles_Agregats_Filter")),
                                    column(2, fluidRow(downloadButton("Poles_Agregats_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Poles_Agregats_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         ),
                         tabPanel("Attracteurs", 
                                  fluidRow(
                                    column(10, plotOutput("Poles_Compo")),
                                    column(2, fluidRow(downloadButton("Poles_Compo_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Poles_Compo_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("Poles_Compo_Filter")),
                                    column(2, fluidRow(downloadButton("Poles_Compo_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Poles_Compo_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         ),
                         tabPanel("Attractivité", 
                                  fluidRow(
                                    column(10, plotOutput("Poles_Attrac")),
                                    column(2, fluidRow(downloadButton("Poles_Attrac_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Poles_Attrac_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("Poles_Attrac_Filter")),
                                    column(2, fluidRow(downloadButton("Poles_Attrac_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Poles_Attrac_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         ),
                         tabPanel("Hiérarchie", 
                                  fluidRow(
                                    column(10, plotOutput("Poles_RT")),
                                    column(2, fluidRow(downloadButton("Poles_RT_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Poles_RT_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("Poles_RT_Filter")),
                                    column(2, fluidRow(downloadButton("Poles_RT_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Poles_RT_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         )
             )),
    tabPanel("Paroisses",
             tabsetPanel(id = "paroissesPlots", 
                         tabPanel("Nombre", 
                                  fluidRow(
                                    column(10, plotOutput("Paroisses_Nb")),
                                    column(2, fluidRow(downloadButton("Paroisses_Nb_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Paroisses_Nb_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("Paroisses_Nb_Filter")),
                                    column(2, fluidRow(downloadButton("Paroisses_Nb_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Paroisses_Nb_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         ),
                         tabPanel("Composition (Nb FP/ paroisse)",
                                  fluidRow(
                                    column(10, plotOutput("Paroisses_Compo")),
                                    column(2, fluidRow(downloadButton("Paroisses_Compo_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Paroisses_Compo_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("Paroisses_Compo_Filter")),
                                    column(2, fluidRow(downloadButton("Paroisses_Compo_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Paroisses_Compo_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         ),
                         tabPanel("Modes de promotion",
                                  fluidRow(
                                    column(10, plotOutput("Paroisses_Promo")),
                                    column(2, fluidRow(downloadButton("Paroisses_Promo_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Paroisses_Promo_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("Paroisses_Promo_Filter")),
                                    column(2, fluidRow(downloadButton("Paroisses_Promo_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Paroisses_Promo_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         ),
                         tabPanel("Superficie", 
                                  fluidRow(
                                    column(10, plotOutput("Paroisses_Superficie")),
                                    column(2, fluidRow(downloadButton("Paroisses_Superficie_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Paroisses_Superficie_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  ),
                                  fluidRow(
                                    column(10,plotOutput("Paroisses_Superficie_Filter")),
                                    column(2, fluidRow(downloadButton("Paroisses_Superficie_Filter_DL", label = "")),
                                           fluidRow(ratingInput(inputId = "Paroisses_Superficie_Filter_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                  )
                         )
             )
    )
  )
  )
)
)
)