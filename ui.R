library(shiny)

function(request){
shinyUI(navbarPage(
  title= "SimEDB", id = "onglet",
  tabPanel(title = "Simulation Exploration",value = "exploration",
           tags$head(
             tags$style(HTML("
                             .ui-resizable-handle{
                             background-attachment: scroll;
                             background-clip: border-box;
                             background-color:  rgb(220,220,220);
                             background-image: url('Splitter-32.png');
                             background-origin: padding-box;
                             background-position: 50% 50%;
                             background-repeat: no-repeat;
                             background-size: auto auto;
                             cursor: col-resize;
                             flex-basis: auto;
                             flex-grow: 0;
                             flex-shrink: 0;
                             font-family: 'Trebuchet MS', 'Lucida Sans Unicode', 'Lucida Grande', 'Lucida Sans', Arial, sans-serif;
                             touch-action: none;
                             }
                             "))
             ),
  sidebarLayout(
    shinyjqui::jqui_resizable(sidebarPanel(width = 4,fluid = TRUE,
                                           fluidRow(textInput("userName", "Utilisateur",  value = "Robin")),
                             fluidRow(selectInput("selectedSims",  label = "Experiences",
                                                   choices = all_sim_names,
                                                  selected = c("6_6_Scenarios", "6_6_Scenarios_base"),
                                                   multiple = TRUE)),
                             textOutput("dataVolumeHaut",inline =  TRUE),
                             fluidRow(column(12, style = "background-color: rgba(67, 162, 202, 0.3);",
                                             plotlyOutput(outputId = "paramPC_Haut", width = "100%", height = "300px")
                                      )),
                             # plotOutput("simNames", height = "200px") %>% withSpinner(type = 7, color = "#C6E3EF"),
                             shiny::checkboxInput(inputId = "show_resultsPlot", value = FALSE, label = "Violin plot ?", width = "50%"),
                             conditionalPanel(
                               condition = "input.show_resultsPlot==true",
                               plotOutput("resultsPlot", height = "200px") %>% withSpinner(type = 7, color = "#C6E3EF")
                             ),
                             textOutput("dataVolumeBas",inline =  TRUE),
                             fluidRow(column(12, style = "background-color: rgba(25, 0, 0, 0.3);",
                                             plotlyOutput(outputId = "paramPC_Bas", width = "100%", height = "300px")
                                             )),
                             fluidRow(
                               column(6, numericInput(inputId = "plotWidth", label = "Largeur" ,value = 20, min = 5, max = 30)),
                               column(6, numericInput(inputId = "plotHeight", label = "Hauteur", value = 10, min = 5, max = 30))
                             )
  ),
  options = list(handles = "e")),
  mainPanel(
    fluidRow(
      textOutput(outputId = "debug")
    ),
    tabsetPanel(id = "detailPlots",type = "pills",
                tabPanel("Objectifs généraux",
                         fluidRow(formattableOutput("summaryTable_Haut", width = "95%")),
                         tags$hr(),
                         fluidRow(formattableOutput("summaryTable_Bas", width = "95%"))
                ),
                tabPanel("Foyers Paysans",
                         tabsetPanel(id = "FPPlots", 
                                     tabPanel(title = "Déplacements",
                                              value = "deplacements",
                                              plotDownloadRateUI("FP_TypeDeplacements_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("FP_TypeDeplacements_Bas", position = "bas")
                                     ),
                                     tabPanel(title = "Déplacements (détail)",
                                              value= "deplacements_detail",
                                              plotDownloadRateUI("FP_DeplacementsDetail_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("FP_DeplacementsDetail_Bas", position = "bas")
                                     ),
                                     tabPanel("Concentration", 
                                              plotDownloadRateUI("FP_Concentration_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("FP_Concentration_Bas", position = "bas")
                                     ),
                                     tabPanel("Satisfaction", 
                                              plotDownloadRateUI("FP_Satisfaction_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("FP_Satisfaction_Bas", position = "bas")
                                     )
                         )),
                tabPanel("Agrégats",
                         tabsetPanel(id = "agregatsPlots",
                                     tabPanel("Nombre", 
                                              plotDownloadRateUI("Agregats_Nb_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Agregats_Nb_Bas", position = "bas")
                                     ),
                                     tabPanel("Distribution",
                                              tags$h4("Distribution des agrégats par classe de taille en fin de simulation"),
                                              tableOutput("Agregats_Distribution_Haut"),
                                              tags$hr(),
                                              tags$h4("Distribution des agrégats par classe de taille en fin de simulation"),
                                              tableOutput("Agregats_Distribution_Bas")
                                              ),
                                     tabPanel("Taille des agrégats principaux",
                                              tags$h4("Tailles des 4 plus importants agrégats (en nombre de foyers paysans)"),
                                              tableOutput("Agregats_Taille_Haut"),
                                              tags$hr(),
                                              tags$h4("Tailles des 4 plus importants agrégats (en nombre de foyers paysans)"),
                                              tableOutput("Agregats_Taille_Bas")
                                     ),
                                     tabPanel("Pôles", 
                                              plotDownloadRateUI("Agregats_Poles_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Agregats_Poles_Bas", position = "bas")
                                     ),
                                     tabPanel("Paroisses", 
                                              plotDownloadRateUI("Agregats_Paroisses_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Agregats_Paroisses_Bas", position = "bas")
                                     ),
                                     tabPanel("Paroisses (composition)", 
                                              plotDownloadRateUI("Agregats_NbParoisses_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Agregats_NbParoisses_Bas", position = "bas")
                                     ),
                                     tabPanel("Communauté", 
                                              plotDownloadRateUI("Agregats_CA_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Agregats_CA_Bas", position = "bas")
                                     ),
                                     tabPanel("Hiérarchie",
                                              plotDownloadRateUI("Agregats_RT_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Agregats_RT_Bas", position = "bas")
                                     ),
                                     tabPanel("Répartition",
                                              plotDownloadRateUI("Agregats_Carte_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Agregats_Carte_Bas", position = "bas")
                                     )
                                     )),
                tabPanel("Seigneurs",
                         tabsetPanel(id = "seigneursPlots",
                                     tabPanel("Nombre", 
                                              plotDownloadRateUI("Seigneurs_Nb_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Seigneurs_Nb_Bas", position = "bas")
                                     ),
                                     # tabPanel("Chateaux", 
                                     #          plotDownloadRateUI("Seigneurs_Chateaux_Haut", position = "haut"),
                                     #          tags$hr(),
                                     #          plotDownloadRateUI("Seigneurs_Chateaux_Bas", position = "bas")
                                     # ),
                                     # tabPanel("Vassaux", 
                                     #          plotDownloadRateUI("Seigneurs_Vassaux_Haut", position = "haut"),
                                     #          tags$hr(),
                                     #          plotDownloadRateUI("Seigneurs_Vassaux_Bas", position = "bas")
                                     # ),
                                     # tabPanel("Redevances (global)",
                                     #          plotDownloadRateUI("Seigneurs_Redevances_Haut", position = "haut"),
                                     #          tags$hr(),
                                     #          plotDownloadRateUI("Seigneurs_Redevances_Bas", position = "bas")
                                     #),
                                     tabPanel("Redevances (détail)",
                                              plotDownloadRateUI("Seigneurs_Redevances_PS_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Seigneurs_Redevances_PS_Bas", position = "bas")
                                     ),
                                     tabPanel("Puissance",
                                              plotDownloadRateUI("Seigneurs_Puissance_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Seigneurs_Puissance_Bas", position = "bas")
                                     )
                         #,
                                     # tabPanel("Agrégats",
                                     #          plotDownloadRateUI("Seigneurs_Agregats_Haut", position = "haut"),
                                     #          tags$hr(),
                                     #          plotDownloadRateUI("Seigneurs_Agregats_Bas", position = "bas")
                                     # )
                         )),
                tabPanel("Châteaux",
                         tabsetPanel(id = "chateauxPlots",
                                     tabPanel("Évolution",
                                              plotDownloadRateUI("Chateaux_Nb_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Chateaux_Nb_Bas", position = "bas")
                                              ),
                                     tabPanel("Détail (constructeurs)",
                                              plotDownloadRateUI("Chateaux_Proprio_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Chateaux_Proprio_Bas", position = "bas")
                                              ),
                                     tabPanel("Détail (type)",
                                              plotDownloadRateUI("Chateaux_Type_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Chateaux_Type_Bas", position = "bas")
                                     )
                           )),
                tabPanel("Pôles",
                         tabsetPanel(id = "polesPlots", 
                                     tabPanel("Nombre", 
                                              plotDownloadRateUI("Poles_Nb_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Poles_Nb_Bas", position = "bas")
                                     ),
                                     tabPanel("Pôles d'agrégats", 
                                              plotDownloadRateUI("Poles_Agregats_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Poles_Agregats_Bas", position = "bas")
                                     ),
                                     tabPanel("Attracteurs", 
                                              plotDownloadRateUI("Poles_Compo_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Poles_Compo_Bas", position = "bas")
                                     ),
                                     tabPanel("Attractivité", 
                                              plotDownloadRateUI("Poles_Attrac_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Poles_Attrac_Bas", position = "bas")
                                     ),
                                     tabPanel("Hiérarchie", 
                                              plotDownloadRateUI("Poles_RT_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Poles_RT_Bas", position = "bas")
                                     ),
                                     tabPanel("Répartition", 
                                              plotDownloadRateUI("Poles_Carte_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Poles_Carte_Bas", position = "bas")
                                     )
                         )),
                tabPanel("Paroisses",
                         tabsetPanel(id = "paroissesPlots", 
                                     tabPanel("Nombre", 
                                              plotDownloadRateUI("Paroisses_Nb_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Paroisses_Nb_Bas", position = "bas")
                                     ),
                                     tabPanel("Hiérarchie", 
                                              plotDownloadRateUI("Paroisses_RT_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Paroisses_RT_Bas", position = "bas")
                                     ),
                                     tabPanel("Composition (Nb FP/ paroisse)",
                                              plotDownloadRateUI("Paroisses_Compo_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Paroisses_Compo_Bas", position = "bas")
                                     ),
                                     tabPanel("Modes de promotion",
                                              plotDownloadRateUI("Paroisses_Promo_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Paroisses_Promo_Bas", position = "bas")
                                     ),
                                     tabPanel("Modes de promotion (cumul)",
                                              plotDownloadRateUI("Paroisses_Promo_CumSum_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Paroisses_Promo_CumSum_Bas", position = "bas")
                                     ),
                                     tabPanel("Superficie", 
                                              plotDownloadRateUI("Paroisses_Superficie_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Paroisses_Superficie_Bas", position = "bas")
                                     ),
                                     tabPanel("Desserte", 
                                              plotDownloadRateUI("Paroisses_Desserte_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Paroisses_Desserte_Bas", position = "bas")
                                     ),
                                     tabPanel("Répartition", 
                                              plotDownloadRateUI("Paroisses_Carte_Haut", position = "haut"),
                                              tags$hr(),
                                              plotDownloadRateUI("Paroisses_Carte_Bas", position = "bas")
                                     )
                         )
                )
                # ,
                # tabPanel("Debug",
                #          # fluidRow(
                #          #   verbatimTextOutput("selected_seeds_Haut")
                #          # ),
                #          # fluidRow(
                #          #   verbatimTextOutput("selected_seeds_Bas")
                #          # )
                #          plotDownloadRateUI("testModule", position = "bas")
                # )
    ))
  )
  ),
  tabPanel(title = "Sensitivity Analysis",
           value = "sensitivity",
           column(width = 5,
                  div(dataTableOutput("sensitivity_summary"), style = "font-size:75%")
                  ),
           column(width =  7,
                  div(id = "sensitivity_plots")
                  )
  )
)
)
}
