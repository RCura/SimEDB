suppressPackageStartupMessages({
  
  # Interactivity
  library(shiny)
  library(shinythemes)
  library(parcoords) # devtools::install_github("timelyportfolio/parcoords", ref="feature/resize")

  # Data wrangling
  library(tidyverse)
  library(stringr)
  library(forcats)
  #library(data.table)

  
  # Plots
  library(gridExtra)
  library(ggthemes)
  
  # Tables
  library(xtable)
  library(formattable) # devtools::install_github("renkun-ken/formattable")

  # Spatial
  #library(sp)
  
})


load("data/sim_data.Rdata")


