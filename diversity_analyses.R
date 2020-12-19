# parks bees diversity exploratory analysis

library(tidyverse)
library(lme4)
library(vegan)

# load data
floral_data <- read.csv("floral_abundances.csv")
site_gps_data <- read.csv("all_sites_2020.csv")
pollinator_data <- read.csv("all_sites_2020_labels_expanded.csv", na.strings=c(""," ","NA"))

# merge floral data and site data tables by site name
merged_df <- (floral_data %>% full_join(site_gps_data))

# reshape longwise to have row for each month|site combo
# new column (key) = month, new column (value) = number of floral units
merged_df_2 <- merged_df %>%
  # simplify column names to represent months before gathering
  rename(july = number_of_floral_units_july) %>%
  rename(august = number_of_floral_units_august) %>%
  gather(key = month, value = number_floral_units, 
         july:august)

# calculate floral richness 
# (count of rows that represent species|month|site)
# a value of -1 for number_floral_units represents absent
# from sytematically placed plots but present at the site.
# here, include these -1 values for richness at the site.
merged_df_summarised_2 <- merged_df_2 %>% 
  group_by(site, month) %>% 
  # remove rows where plant was flowering at site in 
  # other month but not the month specified in 'month'
  filter(number_floral_units != 0)  %>% 
  # count number of rows to get floral species richness
  add_count() %>%
  # add a more descriptive column name
  rename(floral_richness = n) %>%
  # remove these sites until decide what to do with it
  # everett_crowley and china_creek_north_park
  # are pollinator gardens, very different from 
  # a typical park and not necessarily reduced management but 
  # instead active flowering planting management.
  filter(site != "everett_crowley_park") %>% 
  filter(site != "china_creek_north_park")

# calculate sum of all floral units per site per month
# a value of -1 for number_floral_units represents absent
# from sytematically placed plots but present at the site.
# so do not want to include rows where number of floral units = -1
# in the floral unit density summary metric.
merged_df_summarised_3 <- merged_df_summarised_2 %>%
  group_by(site, month) %>%
  filter(number_floral_units > -1) %>%
  mutate(number_floral_units = na_if(number_floral_units, -1)) %>%
  # surveyed 15 plots so divide by 15 to get per m^2 value
  mutate(flowers_per_sq_m = 
           sum(number_floral_units)/15)

# group merged_df_summarised_3 by site and month to have 
# one row per site / month, while retaining management,
# and overall floral abundance and richness metrics 
merged_df_summarised_4 <- merged_df_summarised_3 %>%
  group_by(site, month, management) %>%
  summarise(floral_richness = mean(floral_richness),
            flowers_per_sq_m = mean(flowers_per_sq_m))

# clean pollinator data before merge
pollinator_data_2 <- pollinator_data %>%
  slice(262:1439) %>% # remove ubc farm dates in may and june
  filter(plant_or_pans == "pan trap") %>% # filter out hand netted specimens (from ubc farm) 
  drop_na(order) %>% # filter out rows where order is.na  
  mutate(month = ifelse(row_number() %in% 1:318, "july", "august"))
# add a month column for pollinator data july for first half of matrix, august after
# remember that the last four rows are actually late adds

# join by site and month to add management and floral data to pollinator observations
# reorder factor for figures with parks on the left
merged_df_pollinators <- pollinator_data_2 %>% 
  left_join(merged_df_summarised_4) %>%
  mutate(management = fct_relevel(management, 
                                  "ControlPark", "ReducedPark", 
                                  "Agricultural", "SemiNatural"))

# calculate overall bee abundance per month|site
merged_df_pollinators_bees_only <- merged_df_pollinators %>%  
  filter(subclade == "Anthophila") %>% # filter for bees only
  group_by(index) %>%
  add_count() %>%
  # add a more descriptive column name
  rename(bee_abundance = n) %>%
  # remove this site until decide what to do with it
  # everett_crowley is a pollinator garden, very different from 
  # a typical park and not necessarily reduced management but instead
  # active flowering planting management.
  filter(site != "everett_crowley_park") %>%
  filter(site != "china_creek_north_park")

# reshape for diversity analyses
# mutate, species_name = paste genus and species (so unique name is in one column)
merged_df_pollinators_bees_only$species_name <- paste(
  merged_df_pollinators_bees_only$genus, 
  merged_df_pollinators_bees_only$species)
# species_abundance = n Rows within site of each unique species_name
merged_df_pollinators_bees_only <- merged_df_pollinators_bees_only %>%  
  group_by(site_abbreviation, species_name) %>%
  mutate(species_abundance = n())
# wide format, where each species_name is a column containing species_abundance at each site
bee_diversity_wide <- merged_df_pollinators_bees_only %>%  
  spread(species_name, species_abundance) %>%  
  select(-X, -site, -country, -province, -latitude, -longitude, -survey_date,
         -city, -plant_or_pans, -Start.Number, -End.Number, -index, -name,
         -order, -family, -genus, -species, -sex, -month, -subclade)
View(bee_diversity_wide)

# Species Composition
# Dissimilarities and Distances
# Jaccard index of similarity, where the numerator is the number of 
# species in the set of shared species (present at both sites) and the
# denominator is the number of species in the set present at either site. 