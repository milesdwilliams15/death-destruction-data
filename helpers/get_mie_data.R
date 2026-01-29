# Script to clean and read in the MIE dataset ----

## get_mie_data()
get_mie_data <- function() {
  # the data
  read_csv(
    "https://raw.githubusercontent.com/milesdwilliams15/foreign-figures/refs/heads/main/_data/mie-1.0.csv"
  ) -> dt
  
  # recodes
  dt |>
    mutate(
      action = frcode(
        action == 1 ~ "Threat to use force",
        action == 2 ~ "Threat to blockade",
        action == 3 ~ "Threat to occupy territory",
        action == 4 ~ "Threat to declare war",
        action == 5 ~ "Threat to use CBR weapons",
        action == 6 ~ "Threat to join war",
        action == 7 ~ "Show of force",
        action == 8 ~ "Alert",
        action == 9 ~ "Nuclear alert",
        action == 10 ~ "Mobilization",
        action == 11 ~ "Fortify border",
        action == 12 ~ "Border violation",
        action == 13 ~ "Blockade",
        action == 14 ~ "Occupation of territory",
        action == 15 ~ "Seizure",
        action == 16 ~ "Attack",
        action == 17 ~ "Clash",
        action == 19 ~ "Use of CBR weapons",
        action == 22 ~ "War Battle"
      ),
      hostlev = frcode(
        hostlev == 2 ~ "Threat to use force",
        hostlev == 3 ~ "Display of force",
        hostlev == 4 ~ "Use of force",
        hostlev == 5 ~ "War"
      ),
      date = ym(paste0(styear, "-", stmon)),
      year = styear
    ) -> dt
  
  # return
  dt
}
