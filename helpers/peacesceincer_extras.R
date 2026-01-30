# -------------------------------------------------------------------------
# Functions with extra goodies for {peacesciencer}
# -------------------------------------------------------------------------

library(peacesciencer)

# add_icd_mics() ----------------------------------------------------------

add_icd_mics <- function(
    data, level
) {
  # check for CoW system codes:
  yes <- any(colnames(data) %in% c("ccode", "ccode1"))
  if(!yes) {
    stop("You can only use this function with CoW system codes.")
  }
  
  # whittle MIE data to MIC level:
  dt <- suppressMessages(
    read_csv(
      "https://raw.githubusercontent.com/milesdwilliams15/foreign-figures/refs/heads/main/_data/mie-1.0.csv"
    )
  )
  
  dt |>
    group_by(
      ccode1, ccode2, styear, micnum
    ) |>
    summarize(
      init = max((eventnum == 1), na.rm = T),
      across(fatalmin1:fatalmax2, sum),
      hostlev = max(hostlev),
      .groups = "drop"
    ) |> 
    group_by(micnum) |>
    mutate(
      icdmiconset = (min(styear) == styear) + 0,
      hostlev = max(hostlev)
    ) -> dt
  
  # only keep those of certain hostlev:
  if(!missing(level)) {
    dt |>
      filter(hostlev >= level) -> dt
  }
  
  # some more aggregating:
  dt |>
    group_by(ccode1, ccode2, styear, micnum) |>
    summarize(
      icdmicongoing = 1,
      icdmiconset = max(icdmiconset),
      icdmicongoing_init1 = max(init),
      icdmiconset_init1 = max(init) * icdmiconset,
      across(fatalmin1:fatalmax2, sum),
      .groups = "drop"
    ) -> dt
  
  # the data isn't mirrored:
  bind_rows(
    dt,
    dt |>
      rename(
        ccode2 = ccode1,
        ccode1 = ccode2,
        icdmicongoing_init2 = icdmicongoing_init1,
        icdmiconset_init2 = icdmiconset_init1,
        fatalmin2 = fatalmin1,
        fatalmin1 = fatalmin2,
        fatalmax2 = fatalmax1,
        fatalmax1 = fatalmax2
      )
  ) |> 
    rename(year = styear) |>
    mutate(
      across(everything(), ~ replace_na(.x, 0))
    ) |> 
    select(
      ccode1, 
      ccode2, 
      year, 
      starts_with("gml_"), 
      everything()
    ) -> dt
  
  # if the data is dyadic:
  if(any(colnames(data) %in% c("ccode1"))) {
    data |>
      left_join(
        dt, by = c("ccode1", "ccode2", "year")
      ) -> dt
    
    # fill in missings:
    dt |>
      mutate(
        across(
          starts_with("icdmic") | starts_with("fatal"),
          ~ replace_na(.x, 0)
        )
      ) -> dt
  } else {
    dt |>
      rename(ccode = ccode1) |>
      group_by(ccode, year) |>
      summarize(
        icdmicongoing = 1,
        icdmiconset = max(icdmiconset),
        icdmicongoing_init = max(icdmicongoing_init1),
        icdmiconset_init = max(icdmiconset_init1),
        fatalmin = sum(fatalmin1),
        fatalmax = sum(fatalmax1),
        fatalmin_total = sum(fatalmin1 + fatalmin2),
        fatalmax_total = sum(fatalmax1 + fatalmax2),
        .groups = "drop"
      ) -> dt
    
    # merge:
    data |>
      left_join(
        dt,
        by = c("ccode", "year")
      ) -> dt
    
    # fill missings:
    dt |>
      mutate(
        across(
          starts_with("icdmic") | starts_with("fatal"),
          ~ replace_na(.x, 0)
        )
      ) -> dt
  }
  
  # return
  dt
}


# add_opportunity() ---------------------------------------------------------

get_opportunity_data <- function(data) {
  create_dyadyears(subset_years = 1816:2014) |>
    add_cow_majors() |>
    add_contiguity() |>
    add_capital_distance() -> ddy
  
  logit <- function(x) 1 / (1 + exp(-x))
  
  ddy |>
    mutate(
      contig = ifelse(conttype >= 1, 1, 0),
      majdyad = pmax(cowmaj1, cowmaj2)
    ) |>
    transmute(
      ccode1, ccode2, year,
      contdyads = contig,
      prd = ifelse(contig == 1 | majdyad == 1, 1, 0),
      bcd = logit(
        4.801 + 4.50*contig - 1.051*log(capdist) + 2.901*majdyad)
    ) -> ddy
  
  ddy |>
    group_by(year) |>
    summarize(
      dyads = n(),
      contdyads = sum(contdyads),
      prd = sum(prd),
      bcd = sum(bcd)
    )
}
