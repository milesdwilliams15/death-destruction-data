# -------------------------------------------------------------------------
# Script to create a tidy war-series dataset
# -------------------------------------------------------------------------


# setup -------------------------------------------------------------------

library(tidyverse)
library(socsci)
library(peacesciencer)


# get the data ------------------------------------------------------------

get_war_data <- function(only_wars = T) {
  
  ## MIE data
  source("https://raw.githubusercontent.com/milesdwilliams15/death-destruction-data/refs/heads/main/helpers/get_mie_data.R")
  get_mie_data() -> dt
  
  ## get NMC data (has population and military totals)
  library(peacesciencer)
  create_dyadyears(subset_years = 1816:2014) |>
    add_nmc() -> ext_dt
  
  ## merge with MIE data
  dt |>
    left_join(
      ext_dt,
      by = c("ccode1", "ccode2", "year")
    ) -> dt
  
  ## process the data
  dt |>
    group_by(micnum) |>
    summarize(
      year = min(year),
      hostlev = max(hostlev),
      fatalmin = sum(fatalmin1 + fatalmin2),
      fatalmax = sum(fatalmax1 + fatalmax2),
      events = n(),
      duration = 1/12 + max(endyear + endmon / 12) - min(styear + stmon / 12),
      bellig_pop = sum(tpop1[year == min(year)] + 
                         tpop2[year == min(year)], na.rm = T) * 1000,
      mili_pop = sum(milper1[year == min(year)] +
                       milper2[year == min(year)], na.rm = T) * 1000
    ) -> war_dt
  ext_dt |>
    distinct(ccode1, year, tpop1) |>
    group_by(year) |>
    summarize(
      world_pop = sum(tpop1) * 1000
    ) -> pop_dt
  
  war_dt |>
    left_join(
      pop_dt, 
      by = "year"
    ) -> war_dt
  
  if(only_wars) {
    war_dt |>
      filter(hostlev == "War") -> war_dt
  }
  
  ## return
  war_dt
}