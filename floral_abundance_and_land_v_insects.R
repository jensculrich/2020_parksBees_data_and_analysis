library(tidyverse)

setwd("C:/Users/jensj/Documents/UBC2/Summer_2020/data")

floral_data <- read.csv("floral_abundances.csv")
site_gps_data <- read.csv("all_sites_2020.csv")
str(floral_data)
str(site_gps_data)

# merge tables by site
merged_df <- (floral_data %>% full_join(site_gps_data))

# calculate sum of all floral units per site per month
# edit: sum of all values "number_of_floral_units_x"
# for which the value is greater than 0.
merged_df_summarised <- merged_df %>%
  group_by(site) %>%
  mutate(total_flowers_july = 
              sum(number_of_floral_units_july), 
            total_flowers_august = 
              sum(number_of_floral_units_august))

P <- ggplot(merged_df_summarised, 
            aes(x = management, y = total_flowers_august)) +
  geom_boxplot()
P

## would like to plot both months on the same y axis
## new column = month, new column = total flowers
## rows = total_flowers_july or total_flowers_august
merged_df_summarised_2 <- merged_df_summarised %>%
  gather(key = month, value = total_flowers, 
         total_flowers_july:total_flowers_august) %>%
  mutate(floral_units_per_sq_m = total_flowers/15)

# make (mutate) a new column that has flowers/15

# now try to plot with fill by month
P <- ggplot(merged_df_summarised_2, 
            aes(x = management, y = floral_units_per_sq_m, fill = month)) +
  geom_boxplot()
P

## richness = sum rows for each site for which
## "number_of_floral_units_x" =/= 0 (could be negative or positive)

## edit or simulate: China creek Park, everett crowley park, campbell rd july.

