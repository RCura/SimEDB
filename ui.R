library(shiny)

shinyUI(navbarPage(
  "SimEDB",
  tabPanel(title = "Simulation Exploration",
  sidebarLayout(sidebarPanel(width = 4,fluid = TRUE,
                             column(6,textInput("userName", "Votre nom",  value = "Robin")),
                             column(6, selectInput("selectedSims",  label = "Experiences",
                                                   choices = all_sim_names,
                                                   selected = c("4_4_A", "4_5_A"),
                                                   multiple = TRUE)),
                             textOutput("dataVolumeHaut",inline =  TRUE),
                             fluidRow(column(12, style = "background-color: rgba(67, 162, 202, 0.3);",
                                             parcoordsOutput("paramParCoordsHaut", width = "100%", height = "200px"))),
                             # dataTableOutput("paramLegend"),
                             plotOutput("simNames", height = "200px"),
                             plotOutput("resultsPlot", height = "200px"),
                             textOutput("dataVolumeBas",inline =  TRUE),
                             fluidRow(column(12, style = "background-color: rgba(25, 0, 0, 0.3);",
                                             parcoordsOutput("paramParCoordsBas", width = "100%", height = "200px")))
  ),
  mainPanel(
    # column(10,
    #   
    # ),
    #column(10,
    
    tabsetPanel(id = "detailPlots",type = "pills",
                tabPanel("Objectifs généraux",
                         fluidRow(formattableOutput("summaryTable_Haut", width = "95%")),
                         tags$hr(),
                         fluidRow(formattableOutput("summaryTable_Bas", width = "95%"))
                ),
                tabPanel("Foyers Paysans",
                         tabsetPanel(id = "FPPlots", 
                                     tabPanel("Déplacements", 
                                              fluidRow(
                                                column(10, plotOutput("FP_TypeDeplacements")),
                                                column(2,
                                                       fluidRow(downloadButton("FP_TypeDeplacements_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "FP_TypeDeplacements_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("FP_TypeDeplacements_filter")),
                                                column(2,
                                                       fluidRow(downloadButton("FP_TypeDeplacements_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "FP_TypeDeplacements_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     ),
                                     tabPanel("Déplacements (détail)", 
                                              fluidRow(
                                                column(10, plotOutput("FP_DeplacementsDetail")),
                                                column(2, fluidRow(downloadButton("FP_DeplacementsDetail_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "FP_DeplacementsDetail_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("FP_DeplacementsDetail_Filter")),
                                                column(2, fluidRow(downloadButton("FP_DeplacementsDetail_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "FP_DeplacementsDetail_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     ),
                                     tabPanel("Concentration", 
                                              fluidRow(
                                                column(10, plotOutput("FP_Concentration")),
                                                column(2, fluidRow(downloadButton("FP_Concentration_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "FP_Concentration_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("FP_Concentration_Filter")),
                                                column(2, fluidRow(downloadButton("FP_Concentration_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "FP_Concentration_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     ),
                                     tabPanel("Satisfaction", 
                                              fluidRow(
                                                column(10, plotOutput("FP_Satisfaction")),
                                                column(2, fluidRow(downloadButton("FP_Satisfaction_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "FP_Satisfaction_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("FP_Satisfaction_Filter")),
                                                column(2, fluidRow(downloadButton("FP_Satisfaction_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "FP_Satisfaction_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     )
                         )),
                tabPanel("Agrégats",
                         tabsetPanel(id = "agregatsPlots",
                                     tabPanel("Nombre", 
                                              fluidRow(
                                                column(10, plotOutput("Agregats_Nb")),
                                                column(2, fluidRow(downloadButton("Agregats_Nb_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Agregats_Nb_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("Agregats_Nb_Filter")),
                                                column(2, fluidRow(downloadButton("Agregats_Nb_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Agregats_Nb_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     ),
                                     tabPanel("Pôles", 
                                              fluidRow(
                                                column(10, plotOutput("Agregats_Poles")),
                                                column(2, fluidRow(downloadButton("Agregats_Poles_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Agregats_Poles_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("Agregats_Poles_Filter")),
                                                column(2, fluidRow(downloadButton("Agregats_Poles_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Agregats_Poles_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     ),
                                     tabPanel("Paroisses", 
                                              fluidRow(
                                                column(10, plotOutput("Agregats_Paroisses")),
                                                column(2, fluidRow(downloadButton("Agregats_Paroisses_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Agregats_Paroisses_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("Agregats_Paroisses_Filter")),
                                                column(2, fluidRow(downloadButton("Agregats_Paroisses_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Agregats_Paroisses_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     ),
                                     tabPanel("Communauté", 
                                              fluidRow(
                                                column(10, plotOutput("Agregats_CA")),
                                                column(2, fluidRow(downloadButton("Agregats_CA_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Agregats_CA_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("Agregats_CA_Filter")),
                                                column(2, fluidRow(downloadButton("Agregats_CA_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Agregats_CA_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     ),
                                     tabPanel("Hiérarchie", 
                                              fluidRow(
                                                column(10, plotOutput("Agregats_RT")),
                                                column(2, fluidRow(downloadButton("Agregats_RT_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Agregats_RT_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("Agregats_RT_Filter")),
                                                column(2, fluidRow(downloadButton("Agregats_RT_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Agregats_RT_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     ))),
                tabPanel("Seigneurs",
                         tabsetPanel(id = "seigneursPlots",
                                     tabPanel("Nombre", 
                                              fluidRow(
                                                column(10, plotOutput("Seigneurs_Nb")),
                                                column(2, fluidRow(downloadButton("Seigneurs_Nb_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Seigneurs_Nb_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("Seigneurs_Nb_Filter")),
                                                column(2, fluidRow(downloadButton("Seigneurs_Nb_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Seigneurs_Nb_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     ),
                                     tabPanel("Chateaux", 
                                              fluidRow(
                                                column(10, plotOutput("Seigneurs_Chateaux")),
                                                column(2, fluidRow(downloadButton("Seigneurs_Chateaux_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Seigneurs_Chateaux_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("Seigneurs_Chateaux_Filter")),
                                                column(2, fluidRow(downloadButton("Seigneurs_Chateaux_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Seigneurs_Chateaux_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     ),
                                     tabPanel("Vassaux", 
                                              fluidRow(
                                                column(10, plotOutput("Seigneurs_Vassaux")),
                                                column(2, fluidRow(downloadButton("Seigneurs_Vassaux_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Seigneurs_Vassaux_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("Seigneurs_Vassaux_Filter")),
                                                column(2, fluidRow(downloadButton("Seigneurs_Vassaux_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Seigneurs_Vassaux_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     ),
                                     tabPanel("Redevances (global)",
                                              fluidRow(
                                                column(10, plotOutput("Seigneurs_Redevances")),
                                                column(2, fluidRow(downloadButton("Seigneurs_Redevances_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Seigneurs_Redevances_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("Seigneurs_Redevances_Filter")),
                                                column(2, fluidRow(downloadButton("Seigneurs_Redevances_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Seigneurs_Redevances_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     ),
                                     tabPanel("Redevances (détail)",
                                              fluidRow(
                                                column(10, plotOutput("Seigneurs_Redevances_PS")),
                                                column(2, fluidRow(downloadButton("Seigneurs_Redevances_PS_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Seigneurs_Redevances_PS_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("Seigneurs_Redevances_PS_Filter")),
                                                column(2, fluidRow(downloadButton("Seigneurs_Redevances_PS_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Seigneurs_Redevances_PS_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     ),
                                     tabPanel("Puissance",
                                              fluidRow(
                                                column(10, plotOutput("Seigneurs_Puissance")),
                                                column(2, fluidRow(downloadButton("Seigneurs_Puissance_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Seigneurs_Puissance_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("Seigneurs_Puissance_Filter")),
                                                column(2, fluidRow(downloadButton("Seigneurs_Puissance_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Seigneurs_Puissance_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     ),
                                     tabPanel("Agrégats",
                                              fluidRow(
                                                column(10, plotOutput("Seigneurs_Agregats")),
                                                column(2, fluidRow(downloadButton("Seigneurs_Agregats_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Seigneurs_Agregats_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("Seigneurs_Agregats_Filter")),
                                                column(2, fluidRow(downloadButton("Seigneurs_Agregats_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Seigneurs_Agregats_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     )
                         )),
                tabPanel("Pôles",
                         tabsetPanel(id = "polesPlots", 
                                     tabPanel("Nombre", 
                                              fluidRow(
                                                column(10, plotOutput("Poles_Nb")),
                                                column(2, fluidRow(downloadButton("Poles_Nb_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Poles_Nb_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("Poles_Nb_Filter")),
                                                column(2, fluidRow(downloadButton("Poles_Nb_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Poles_Nb_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     ),
                                     tabPanel("Pôles d'agrégats", 
                                              fluidRow(
                                                column(10, plotOutput("Poles_Agregats")),
                                                column(2, fluidRow(downloadButton("Poles_Agregats_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Poles_Agregats_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("Poles_Agregats_Filter")),
                                                column(2, fluidRow(downloadButton("Poles_Agregats_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Poles_Agregats_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     ),
                                     tabPanel("Attracteurs", 
                                              fluidRow(
                                                column(10, plotOutput("Poles_Compo")),
                                                column(2, fluidRow(downloadButton("Poles_Compo_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Poles_Compo_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("Poles_Compo_Filter")),
                                                column(2, fluidRow(downloadButton("Poles_Compo_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Poles_Compo_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     ),
                                     tabPanel("Attractivité", 
                                              fluidRow(
                                                column(10, plotOutput("Poles_Attrac")),
                                                column(2, fluidRow(downloadButton("Poles_Attrac_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Poles_Attrac_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("Poles_Attrac_Filter")),
                                                column(2, fluidRow(downloadButton("Poles_Attrac_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Poles_Attrac_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     ),
                                     tabPanel("Hiérarchie", 
                                              fluidRow(
                                                column(10, plotOutput("Poles_RT")),
                                                column(2, fluidRow(downloadButton("Poles_RT_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Poles_RT_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("Poles_RT_Filter")),
                                                column(2, fluidRow(downloadButton("Poles_RT_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Poles_RT_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     )
                         )),
                tabPanel("Paroisses",
                         tabsetPanel(id = "paroissesPlots", 
                                     tabPanel("Nombre", 
                                              fluidRow(
                                                column(10, plotOutput("Paroisses_Nb")),
                                                column(2, fluidRow(downloadButton("Paroisses_Nb_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Paroisses_Nb_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("Paroisses_Nb_Filter")),
                                                column(2, fluidRow(downloadButton("Paroisses_Nb_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Paroisses_Nb_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     ),
                                     tabPanel("Composition (Nb FP/ paroisse)",
                                              fluidRow(
                                                column(10, plotOutput("Paroisses_Compo")),
                                                column(2, fluidRow(downloadButton("Paroisses_Compo_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Paroisses_Compo_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("Paroisses_Compo_Filter")),
                                                column(2, fluidRow(downloadButton("Paroisses_Compo_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Paroisses_Compo_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     ),
                                     tabPanel("Modes de promotion",
                                              fluidRow(
                                                column(10, plotOutput("Paroisses_Promo")),
                                                column(2, fluidRow(downloadButton("Paroisses_Promo_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Paroisses_Promo_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("Paroisses_Promo_Filter")),
                                                column(2, fluidRow(downloadButton("Paroisses_Promo_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Paroisses_Promo_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     ),
                                     tabPanel("Superficie", 
                                              fluidRow(
                                                column(10, plotOutput("Paroisses_Superficie")),
                                                column(2, fluidRow(downloadButton("Paroisses_Superficie_Haut_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Paroisses_Superficie_Haut_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              ),
                                              tags$hr(),
                                              fluidRow(
                                                column(10,plotOutput("Paroisses_Superficie_Filter")),
                                                column(2, fluidRow(downloadButton("Paroisses_Superficie_Bas_DL", label = "")),
                                                       fluidRow(ratingInput(inputId = "Paroisses_Superficie_Bas_Rate",label = "", dataStart = 0, dataStop = 5, dataStep = 1, dataFractions = 1)))
                                              )
                                     )
                         )
                )
    )
  )
  )
  ),
  tabPanel(title = "Sensitivity Analysis",
           column(width = 6, div(dataTableOutput("sensitivity_summary"), style = "font-size:75%")),
           column(width =  6, div(id = "sensitivity_plots"))
  )
)
)