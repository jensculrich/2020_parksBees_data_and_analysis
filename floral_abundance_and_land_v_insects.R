library(tidyverse)

# load data
floral_data <- read.csv("floral_abundances.csv")
site_gps_data <- read.csv("all_sites_2020.csv")

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
  rename(floral_richness = n)

# plot floral species richness my month|site
Q <- ggplot(merged_df_summarised_2, 
            aes(x = management, y = floral_richness)) +
  geom_boxplot(aes(fill = forcats::fct_rev(month))) +
  theme_classic()
Q


# calculate sum of all floral units per site per month
# a value of -1 for number_floral_units represents absent
# from sytematically placed plots but present at the site.
# so do not want to include rows where number of floral units = -1.
merged_df_summarised_3 <- merged_df_summarised_2 %>%
  group_by(site, month) %>%
  filter(number_floral_units > -1) %>%
  mutate(number_floral_units = na_if(number_floral_units, -1)) %>%
  # surveyed 15 plots so divide by 15 to get per m^2 value
  mutate(flowers_per_sq_m = 
           sum(number_floral_units)/15) %>%
  # remove this site until decide what to do with it
  # everett_crowley is a pollinator garden, very different from 
  # a typical park and not necessarily reduced management but instead
  # active flowering planting management.
  filter(site != "everett_crowley_park")

# plot floral abundance my month|site
P <- ggplot(merged_df_summarised_3, 
            aes(x = management, y = flowers_per_sq_m, fill = month)) +
  geom_boxplot() +
  theme_classic()
P

# use a two-sample t-test to compare flowers per sq m in reduced v control parks
# first filter out the agricultural and semi nat sites
df_parks_subset_july <- merged_df_summarised_3 %>%
  filter(management == "ReducedPark" | management == "ControlPark") %>%
  filter(month == "july")
df_parks_subset_august <- merged_df_summarised_3 %>%
  filter(management == "ReducedPark" | management == "ControlPark") %>%
  filter(month == "august")

t.test(flowers_per_sq_m ~ management, data = df_parks_subset_july)
# there are more flowers/m^2 in reduced management parks in july
t.test(flowers_per_sq_m ~ management, data = df_parks_subset_august)
# there is no difference in flowers/m^2 between mangement approaches in august
t.test(floral_richness ~ management, data = df_parks_subset_july)
t.test(floral_richness ~ management, data = df_parks_subset_august)
# there is a higher richness of flowering species near 
# the survey location at reduced management parks 
# in july and also in august

# edit or simulate: China creek Park (july: counted 1/3 plots in the pollinator garden),
# everett crowley park (july and august: counted plots in pollinator garden), 
# campbell rd (july: did not visit).

