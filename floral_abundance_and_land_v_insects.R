library(tidyverse)

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
  # remove this site until decide what to do with it
  # everett_crowley is a pollinator garden, very different from 
  # a typical park and not necessarily reduced management but instead
  # active flowering planting management.
  filter(site != "everett_crowley_park")

# plot floral species richness my month|site
Q <- ggplot(merged_df_summarised_2, 
            aes(x = management, y = floral_richness)) +
  geom_boxplot(aes(fill = forcats::fct_rev(month))) +
  theme_classic()
Q

# plot floral species richness my month|site for parks only
Q2 <- ggplot(filter(merged_df_summarised_3, 
                    management == "ReducedPark" | management == "ControlPark"), 
            aes(x = management, y = floral_richness)) +
  geom_boxplot(aes(fill = forcats::fct_rev(month))) +
  theme_classic()
Q2


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
           sum(number_floral_units)/15)

# plot floral abundance by month|site
P <- ggplot(merged_df_summarised_3, 
            aes(x = management, y = flowers_per_sq_m, fill = month)) +
  geom_boxplot(aes(fill = forcats::fct_rev(month))) +
  theme_classic()
P

# plot floral abundance by month|site for parks only
P2 <- ggplot(filter(merged_df_summarised_3, 
                    management == "ReducedPark" | management == "ControlPark"), 
            aes(x = management, y = flowers_per_sq_m, fill = month)) +
  geom_boxplot(aes(fill = forcats::fct_rev(month))) +
  theme_classic()
P2

# use a two-sample t-test to compare flowers per sq m in reduced v control parks
# first filter out the agricultural and semi nat sites
df_parks_subset_july <- merged_df_summarised_3 %>%
  filter(management == "ReducedPark" | management == "ControlPark") %>%
  filter(month == "july")
df_parks_subset_august <- merged_df_summarised_3 %>%
  filter(management == "ReducedPark" | management == "ControlPark") %>%
  filter(month == "august")

# use Welch two sample t-test to compare the means of abundance
t.test(flowers_per_sq_m ~ management, data = df_parks_subset_july)
# there are more flowers/m^2 in reduced management parks in july
t.test(flowers_per_sq_m ~ management, data = df_parks_subset_august)
# there is no difference in flowers/m^2 between mangement approaches in august

# use Welch two sample t-test to compare the means of species richness
t.test(floral_richness ~ management, data = df_parks_subset_july)
t.test(floral_richness ~ management, data = df_parks_subset_august)
# there is a higher richness of flowering species near 
# the survey location at reduced management parks 
# in july and also in august

# edit or simulate: China creek Park (july: counted 1/3 plots in the pollinator garden),
# everett crowley park (july and august: counted plots in pollinator garden), 
# campbell rd (july: did not visit).

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
  mutate(month = ifelse(row_number() %in% 1:214, "july", "august"))
  
  # add a month column for pollinator data july for first half of matrix, august after
  # remember that the last four rows are actually late adds

# join by site and month to add management and floral data to pollinator observations
merged_df_pollinators <- (pollinator_data_2 %>% 
                            left_join(merged_df_summarised_4))

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
  

# plot bee abundance by month|site
R <- ggplot(merged_df_pollinators_bees_only, 
            aes(x = management, y = bee_abundance, fill = month)) +
  geom_boxplot(aes(fill = forcats::fct_rev(month))) +
  theme_classic()
R

# plot bee abundance by month|site for parks only
R2 <- ggplot(filter(merged_df_pollinators_bees_only, 
                    management == "ReducedPark" | management == "ControlPark"), 
             aes(x = management, y = bee_abundance, fill = month)) +
  geom_boxplot(aes(fill = forcats::fct_rev(month))) +
  theme_classic()
R2

# plot bee abundance by flowers per sq m
R3 <- ggplot(filter(merged_df_pollinators_bees_only, 
                    management == "ReducedPark" | management == "ControlPark"), 
             aes(x = flowers_per_sq_m, y = bee_abundance)) +
  geom_point(aes(colour = management, shape = month), size = 5) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) +
  theme_classic()
R3

# filter df for parks only
parks_bees <- filter(merged_df_pollinators_bees_only, 
       management == "ReducedPark" | management == "ControlPark")

# fit linear regression for log transformed data
log_log_abundance_model <- lm(data = parks_bees, 
                              log(bee_abundance) ~ log(flowers_per_sq_m))
summary(log_log_abundance_model)

# fit linear regression for log transformed data w random effect
log_log_abundance_mixed_model <- lme4::lmer(data = parks_bees, 
                              log(bee_abundance) ~ log(flowers_per_sq_m) + 
                                (1|management))
summary(log_log_abundance_mixed_model)
anova(log_log_abundance_mixed_model, log_log_abundance_model)

# plot bee abundance by flowers per sq m as a log log
R4 <- ggplot(parks_bees, 
             aes(x = log(flowers_per_sq_m), y = log(bee_abundance))) +
  geom_point(aes(colour = management, shape = month), size = 5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()
R4

hist(parks_bees$bee_abundance, main = "", breaks = 100, col = "grey", border = "grey")
hist(log(parks_bees$bee_abundance), main = "", breaks = 50, col = "grey", border = "grey")
abline(v = log(mean(parks_bees$bee_abundance)), col = "red", lwd = 2)   
abline(v = mean(log(parks_bees$bee_abundance)), col = "blue", lwd = 2)


# use Welch two sample t-test to compare the means of abundance
merged_df_pollinators_bees_only_july <- merged_df_pollinators_bees_only %>% 
  filter(management == "ControlPark" | management == "ReducedPark") %>%
  filter(month == "july")  
  
  t.test(bee_abundance ~ management, data = merged_df_pollinators_bees_only_july)
  # there are more bees per site in reduced management parks in july

merged_df_pollinators_bees_only_august <- merged_df_pollinators_bees_only %>% 
  filter(management == "ControlPark" | management == "ReducedPark") %>%
  filter(month == "august")  
  
  t.test(bee_abundance ~ management, data = merged_df_pollinators_bees_only_august)
  # there are more bees per site in reduced management parks in august

# repeat with fly data  
merged_df_pollinators_syrphid_only <- merged_df_pollinators %>%  
  filter(family == "Syrphidae") %>% # filter for bees only
  group_by(index) %>%
  add_count() %>%
  # add a more descriptive column name
  rename(syrphid_abundance = n) %>%
  # remove this site until decide what to do with it
  # everett_crowley is a pollinator garden, very different from 
  # a typical park and not necessarily reduced management but instead
  # active flowering planting management.
  filter(site != "everett_crowley_park") %>%
  filter(site != "china_creek_north_park")

# plot syrphid fly abundance by month|site
S <- ggplot(merged_df_pollinators_syrphid_only, 
            aes(x = management, y = syrphid_abundance, fill = month)) +
  geom_boxplot(aes(fill = forcats::fct_rev(month))) +
  theme_classic()
S

# calculate overall wasp abundance per month|site
merged_df_pollinators_wasps_only <- merged_df_pollinators %>%  
  filter(subclade == "not_Anthophila") %>% # filter for non-bee wasps only
  group_by(index) %>%
  add_count() %>%
  # add a more descriptive column name
  rename(total_wasp_abundance = n) %>%
  # add column 1 if family is Vespidae or 0 if not Vespidae 
  mutate(is_Vespidae = ifelse(family == "Vespidae", "Vespidae", "other Hymenoptera")) %>%
  # remove this site until decide what to do with it
  # everett_crowley is a pollinator garden, very different from 
  # a typical park and not necessarily reduced management but instead
  # active flowering planting management.
  group_by(index, is_Vespidae) %>%
  add_count() %>%
  # add a more descriptive column name
  rename(wasp_type_abundance = n) %>%
  filter(site != "everett_crowley_park") %>%
  filter(site != "china_creek_north_park")

# plot wasp fly abundance by month|site
T <- ggplot(merged_df_pollinators_wasps_only, 
            aes(x = management, y = wasp_type_abundance, fill = month)) +
  geom_boxplot(aes(fill = forcats::fct_rev(month))) +
  facet_wrap(~is_Vespidae) +
  theme_classic()
T

