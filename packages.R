suppressPackageStartupMessages({
  # Data wrangling
  library(tidyverse)
  library(magrittr)
  library(lubridate)
  library(glue)
  
  # DataBase
  library(dbplyr)
  library(DBI)
  library(RJDBC)
  
  # Interactivity
  library(shiny)
  library(shinythemes)
  library(ShinyRatingInput) # devtools::install_github("stefanwilhelm/ShinyRatingInput")
  library(shinyjqui) # Resizable on sidebar
  
  # Plots
  library(gridExtra) # Plots en 2-3 sous-plots
  library(ggthemes) # geom_tufteboxplot
  
  # Table
  library(xtable)
  library(formattable) # Main table objectifs
  library(DT) # Sensitivity table
  library(shinycssloaders) # CSS loaders on plots
  
  # Interactive Plots
  library(plotly) # Parallel coordinates plot
  library(htmlwidgets) # Custom call to onRender() for parallel coordinates
  library(jsonlite) # Handling htmlwidgets::onRender answer
})