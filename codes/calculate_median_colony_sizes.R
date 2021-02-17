# calculate mean colony size by island
# load libraries
library(data.table)

# load data
source("codes/initial_survey_formatting.R")

df <- rbindlist(list(
  esd0812[, c("Island", "Family", "Colonylength")],
  esd1317[, c("Island", "Family", "Colonylength")],
  hicordis[, c("Island", "Family", "Colony_length")]
  ), 
  use.names=F
  )

median_sizes <- df %>%
  group_by(Island, Family) %>%
  summarize(Island_median_colony_size = median(Colonylength, na.rm = T)) %>%
  filter(Family == "Acroporidae" | Family == "Poritidae")

save(median_sizes, file = "Compiled_data/median_colony_sizes_by_island.RData")
