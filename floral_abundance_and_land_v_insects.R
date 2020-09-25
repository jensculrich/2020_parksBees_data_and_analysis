library(tidyverse)

setwd("C:/Users/jensj/Documents/UBC2/Summer_2020/data")

floral_data <- read.csv("floral_abundances.csv")
site_gps_data <- read.csv("all_sites_2020.csv")
str(floral_data)
str(site_gps_data)

# merge tables by site
merged_df <- (floral_data %>% full_join(site_gps_data))

# new column (key) = month, new column (value) = number of floral units
merged_df_2 <- merged_df %>%
  rename(july = number_of_floral_units_july) %>%
  rename(august = number_of_floral_units_august) %>%
  gather(key = month, value = number_floral_units, 
         july:august)

# merged_df_summarised
# calculate sum of all floral units per site per month
# edit: sum of all values "number_floral_units_x"
# for which the value is greater than 0.
merged_df_summarised <- merged_df_2 %>%
  group_by(site, month) %>%
  mutate(flowers_per_sq_m = 
              sum(number_floral_units)/15) 
# counted 15 plots so divide by 15 to get per m^2

P <- ggplot(merged_df_summarised, 
            aes(x = management, y = flowers_per_sq_m, fill = month)) +
  geom_boxplot()
P

merged_df_summarised_2 <- merged_df_summarised %>% 
  group_by(site, month) %>% 
  # remove rows where plant was flowering at site in 
  # other month but not the month specified in 'month'
  filter(number_floral_units != 0)  %>% 
  # count number of rows to get floral species richness
  add_count() %>%
  rename(floral_richness = n)

Q <- ggplot(merged_df_summarised_2, 
            aes(x = management, y = floral_richness)) +
  geom_boxplot(aes(fill = forcats::fct_rev(month)))
Q

merged_df_summarised_3 <- merged_df_summarised_2 %>%
  group_by(site, month) %>%
  filter(number_floral_units > -1) %>%
  mutate(number_floral_units = na_if(number_floral_units, -1)) %>%
  mutate(flowers_per_sq_m = 
           sum(number_floral_units)/15)

P2 <- ggplot(merged_df_summarised_3, 
            aes(x = management, y = flowers_per_sq_m, fill = month)) +
  geom_boxplot()
P2

# edit or simulate: China creek Park, everett crowley park, campbell rd july.

