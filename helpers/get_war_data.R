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
  suppressMessages(get_mie_data()) -> dt
  
  ## get NMC data (has population and military totals)
  suppressMessages(create_dyadyears(subset_years = 1816:2014) |>
    add_nmc()) -> ext_dt
  
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
      n_participants = c(unique(ccode1), unique(ccode2)) |> 
        n_distinct()
    ) -> war_dt
  
  ## pop data
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
  
  ## participant pop and mil data
  dt |>
    distinct(ccode1, ccode2, year, micnum, tpop1, tpop2, milper1, milper2, milex1, milex2) -> dst
  dst |>
    select(
      year, micnum, ends_with("1")
    ) -> dst1
  dst |>
    select(
      year, micnum, ends_with("2")
    ) -> dst2
  bind_rows(
    dst1 |> 
      rename_with(~ str_remove_all(.x, "1")), 
    dst2 |>
      rename_with(~ str_remove_all(.x, "2"))
  ) |>
    distinct() |>
    group_by(micnum, ccode) |>
    filter(year == min(year)) |>
    group_by(micnum) |>
    summarize(
      year = min(year),
      bellig_pop = sum(tpop) * 1000,
      mil_pop = sum(milper, na.rm = T) * 1000,
      mil_exp = sum(milex, na.rm = T) * 1000
    ) -> dst
  
  war_dt |>
    left_join(
      dst,
      by = c("micnum", "year")
    ) -> war_dt
  
  ## opportunity data
  source("https://raw.githubusercontent.com/milesdwilliams15/death-destruction-data/refs/heads/main/helpers/peacesciencer_extras.R")
  suppressMessages(get_opportunity_data()) -> opp_dt
  
  war_dt |>
    left_join(
      opp_dt,
      by = "year"
    ) -> war_dt
  
  if(only_wars) {
    war_dt |>
      filter(hostlev == "War") -> war_dt
  }
  
  ## return
  war_dt
}


# compute yearly war propensity -------------------------------------------

summarize_war_prop <- function(data, level = NULL, opp = NULL) {
  
  micnum_year <- any(colnames(data) == "micnum") &
    any(colnames(data) == "year")
  if(!micnum_year) stop("Columns 'micnum' and 'year' not detected.")
  
  level_filter <- !is.null(level)
  opp_selected <- !is.null(opp)
  
  if(level_filter) data |>
    filter(hostlev >= level) -> data
  
  if(opp_selected) {
    data |>
      group_by(year) |>
      summarize(
        war_prop = n() / unique(.data[[opp]])
      ) -> out
  } else {
    data |>
      group_by(year) |>
      summarize(
        war_prop = n() / unique(prd)
      ) -> out
  }
  
  out |>
    mutate(
      measure = ifelse(
        level_filter, level, "All"
      )
    ) -> out
  
  ## expand and return
  out %>%
    complete(
      year = 1816:2014, 
      fill = list(
        war_prop = 0,
        measure = unique(.[["measure"]])
      )
    )
}


# binning and bootstrapping -----------------------------------------------

mean_ci_boot <- function(data, var, bin = 20, ci = .84) {
  data |>
    # add numerical labels for each 20 year period
    mutate(
      period_n = floor(1:n() / bin) * bin
    ) |>
    # add labels that specify the year range per period
    group_by(period_n) |>
    mutate(
      period_label = paste0(min(year), "-", max(year))
    ) |>
    # make sure the labels are treated as an ordered category
    ungroup() |>
    mutate(
      period = factor(
        period_n,
        levels = period_n,
        labels = period_label
      )
    ) |>
    # get the mean and CIs by period label
    group_by(period) -> data
  
    mean.ci.boot <- function(var, ci) {
      vbar <- mean(var, na.rm = T)
      vboot <- replicate(
        n = 1000,
        expr = mean(sample(var, length(var), T), na.rm = T)
      )
      data.frame(
        mean = vbar,
        lower = quantile(vboot, .5 - ci / 2),
        upper = quantile(vboot, .5 + ci / 2)
      )
    }
    
    data |>
      group_by(period) |>
      summarize(
        mean.ci.boot(.data[[var]], ci = ci)
      )
}

