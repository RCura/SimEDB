suppressPackageStartupMessages({
  
  # Interactivity
  library(shiny)
  library(shinythemes)
  library(parcoords) # devtools::install_github("timelyportfolio/parcoords", ref="feature/resize")

  # Data wrangling
  library(readr)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(magrittr)
  library(data.table)
  
  # Plots
  library(ggplot2)
  library(gridExtra)
  library(ggthemes)
  
  # Tables
  library(xtable)
  library(formattable) # devtools::install_github("renkun-ken/formattable")

  # Spatial
  library(sp)
  
})


load("data/JIAP_data.Rdata")

