library(shiny)

shinyUI(fluidPage(theme = shinytheme("cerulean"),
  fluidRow(
    column(8, titlePanel("SimVADB")),
    column(4,fileInput("simResultsFiles", label = "Sorties de simulation", multiple = TRUE))
  ),             
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
                                              column(6,plotOutput("fpTypeDeplacements")),
                                              column(6,plotOutput("fpTypeDeplacementsFilter"))
                                            )),
                                   tabPanel("Déplacements (détail)", 
                                            fluidRow(
                                              column(6,plotOutput("fpDeplacementsDetail")),
                                              column(6,plotOutput("fpDeplacementsDetailFilter"))
                                            )),
                                   tabPanel("Concentration",
                                            fluidRow(
                                              column(6,plotOutput("fpConcentration")),
                                              column(6,plotOutput("fpConcentrationFilter"))
                                            )),
                                   tabPanel("Satisfaction",
                                            fluidRow(
                                              column(6,plotOutput("fpSatisfaction")),
                                              column(6,plotOutput("fpSatisfactionFilter"))
                                            ))
                       )),
              tabPanel("Agrégats",
                       tabsetPanel(id = "agregatsPlots",
                                   tabPanel("Nombre",
                                            fluidRow(
                                              column(6,plotOutput("agregatsNb")),
                                              column(6, plotOutput("agregatsNbFilter"))
                                              )),
                                   tabPanel("Pôles",
                                            fluidRow(
                                              column(6,plotOutput("agregatsPoles")),
                                              column(6, plotOutput("agregatsPolesFilter"))
                                            )),
                                   tabPanel("Communauté",
                                            fluidRow(
                                              column(6,plotOutput("agregatsCA")),
                                              column(6, plotOutput("agregatsCAFilter"))
                                            )),
                                    tabPanel("Hiérarchie",
                                             fluidRow(
                                               column(6,plotOutput("agregatsRT")),
                                               column(6, plotOutput("agregatsRTFilter"))
                                              ))
                                   )),
              tabPanel("Seigneurs",
                       tabsetPanel(id = "seigneursPlots",
                                   tabPanel("Chateaux",
                                            fluidRow(
                                              column(6,plotOutput("seigneursChateaux")),
                                              column(6, plotOutput("seigneursChateauxFilter"))
                                            )),
                                   tabPanel("Vassaux",
                                            fluidRow(
                                              column(6,plotOutput("seigneursVassaux")),
                                              column(6, plotOutput("seigneursVassauxFilter"))
                                            )),
                                   tabPanel("Redevances",
                                            fluidRow(
                                              column(6,plotOutput("seigneursRedevances")),
                                              column(6, plotOutput("seigneursRedevancesFilter"))
                                            )),
                                   tabPanel("Puissance",
                                            fluidRow(
                                              column(6,plotOutput("seigneursPuissance")),
                                              column(6, plotOutput("seigneursPuissanceFilter"))
                                            ))
                       )),
              tabPanel("Pôles",
                       tabsetPanel(id = "polesPlots",
                                   tabPanel("Nombre",
                                            fluidRow(
                                              column(6,plotOutput("polesNb")),
                                              column(6, plotOutput("polesNbFilter"))
                                            )),
                                   tabPanel("Pôles d'agrégats",
                                            fluidRow(
                                              column(6,plotOutput("polesAgregats")),
                                              column(6, plotOutput("polesAgregatsFilter"))
                                            )),
                                   tabPanel("Attracteurs",
                                            fluidRow(
                                              column(6,plotOutput("polesCompo")),
                                              column(6, plotOutput("polesCompoFilter"))
                                            )),
                                   tabPanel("Attractivité",
                                            fluidRow(
                                              column(6,plotOutput("polesAttrac")),
                                              column(6, plotOutput("polesAttracFilter"))
                                            )),                                   
                                   tabPanel("Hiérarchie",
                                            fluidRow(
                                              column(6,plotOutput("polesRT")),
                                              column(6, plotOutput("polesRTFilter"))
                                            ))
                       )),
              tabPanel("Paroisses",
                       tabsetPanel(id = "paroissesPlots",
                                   tabPanel("Nombre",
                                            fluidRow(
                                              column(6,plotOutput("paroissesNb")),
                                              column(6, plotOutput("paroissesNbFilter"))
                                            )),
                                   tabPanel("Composition (Nb FP/ paroisse)",
                                            fluidRow(
                                              column(6,plotOutput("paroissesCompo")),
                                              column(6, plotOutput("paroissesCompoFilter"))
                                            )),
                                   tabPanel("Modes de promotion",
                                            fluidRow(
                                              column(6,plotOutput("paroissesPromo")),
                                              column(6, plotOutput("paroissesPromoFilter"))
                                            )),
                                   tabPanel("Superficie",
                                            fluidRow(
                                              column(6,plotOutput("paroissesSuperficie")),
                                              column(6, plotOutput("paroissesSuperficieFilter"))
                                            ))
                       ))
              )
))
