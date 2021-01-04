# parks bees diversity exploratory analysis

library(tidyverse)
library(lme4)
library(vegan)
library(proxy)

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
  rename(floral_richness = n) 

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
  # remove these sites until decide what to do with it
  # everett_crowley and china_creek_north_park
  # are pollinator gardens, very different from 
  # a typical park and not necessarily reduced management but 
  # instead active flowering planting management.
  filter(site != "everett_crowley_park") %>% 
  filter(site != "china_creek_north_park") %>% 
  # remove site only visited once not twice
  filter(site != "campbell_road") %>%
  # remove ubc farm sites (organic, small, diverse, surrounded by forest)
  filter(site != "ubc_farm_hedgerow") %>%
  filter(site != "campbell_field_margin")

# reshape for diversity analyses
# mutate, species_name = paste genus and species (so unique name is in one column)
merged_df_pollinators_bees_only$species_name <- paste(
  merged_df_pollinators_bees_only$genus, 
  merged_df_pollinators_bees_only$species)
# species_abundance = n Rows within site of each unique species_name
merged_df_pollinators_bees_only <- merged_df_pollinators_bees_only %>%  
  group_by(site_abbreviation, species_name) %>%
  mutate(species_abundance = 1) # create a marker for each row that will be tallied later

# wide format, where each species_name is a column containing species_abundance at each site
bee_diversity_wide <- merged_df_pollinators_bees_only %>%  
  spread(species_name, species_abundance) %>% # spread wide with row per occurrence of each species
  select(-X, -site, -country, -province, -latitude, -longitude, -survey_date,
         -city, -plant_or_pans, -Start.Number, -End.Number, -index, -name,
         -order, -family, -genus, -species, -sex, -month, -subclade) %>% # drop unneccessary columns
  replace(., is.na(.), 0) %>% # replace NA with 0
  # condense to one row per site
  group_by(site_abbreviation) %>% 
  summarise_at(vars(6:46), sum, na.rm = TRUE)  %>% # sums tally within species to get species abundance per site
  filter(site_abbreviation != "china", site_abbreviation != "everett",
         site_abbreviation != "ubc_f1", site_abbreviation != "ubc_f2", 
         site_abbreviation != "campbell")


# merge with merged_df_pollinators_bees_only to get management
# get one row per site with corresponidng management type data
merged_df_pollinators_bees_only_2 <- merged_df_pollinators_bees_only %>%
  group_by(site_abbreviation) %>%
  filter(row_number()==1) %>%
  select(site_abbreviation, management) 
# join the management to the diversity by site name
bee_diversity <- bee_diversity_wide %>%
  left_join(merged_df_pollinators_bees_only_2,
            by = "site_abbreviation") %>%
  select(-"Apis mellifera") # removed honey bees for diversity, should I reconsider?

View(bee_diversity)

# Species Composition
# Dissimilarities and Distances
# Jaccard index of similarity, where the numerator is the number of 
# species in the set of shared species (present at both sites) and the
# denominator is the number of species in the set present at either site. 
# summarize by management type first
bee_diversity_summarized <- bee_diversity %>%
  group_by(management) %>%
  summarise_at(vars(2:42), sum, na.rm = TRUE) %>%
  select(-management)
# 1 = control, 2 = reduced, 3 = agricultural, 4 = semi-natural
dist(bee_diversity_summarized, by_rows = TRUE, method = "Jaccard")
# Jaccard distance lowest between park types and highest between 
# reduced parks and semi-nat/ag
dist(bee_diversity_summarized, by_rows = TRUE, method = "Euclidean")
# euclidean distance = sqrt(X1^2 + X2^2 + X3^2 ... + Xn^2) where Xn equals 
# the difference in abundance of a species between two sites
# euclidean distance (squared values) tend to emphasize differences in the most abundant species
# euclidean distance lowest between control parks and semi-nat and 
# highest between reduced parks and semi-nat/ag
vegdist(bee_diversity_summarized)
# bray curtis distance = abs(total difference in species abund between 2 sites), 
# divided by the total abundances at each site. emphasizes rare and common species more equally.
# bray-curtiss distance lowest between park types
# bray-curtiss lowest betwen park types

# could also calculate composition based on relative density, where each 
# species in a sample is represented by proportion of the sample comprised of
# that species. Relative abundance prob better since number of sites per category (and thus abundances) are highly variable.
# For management type, divide each species by sum of all species.
for(i in 1:4){
  bee_diversity_summarized[i,] <- bee_diversity_summarized[i,] / 
    sum(bee_diversity_summarized[i,])
}

# 1 = control, 2 = reduced, 3 = agricultural, 4 = semi-natural
dist(bee_diversity_summarized, by_rows = TRUE, method = "Jaccard")
# Jaccard distance lowest between park types and highest between 
# reduced parks and semi-nat/ag
dist(bee_diversity_summarized, by_rows = TRUE, method = "Euclidean")
# euclidean distance lowest between park types and 
# highest between both park types and agricultural
vegdist(bee_diversity_summarized)
# bray-curtiss distance lowest between park types and
# highest between parks and semi-nat, maybe bray 
# add statistical test for sign of bray-curtiss disimilarity? #

# 1 = control, 2 = reduced, 3 = agricultural, 4 = semi-natural
mdsE <- metaMDS(bee_diversity_summarized, distance="euc", autotransform=FALSE, trace=0)
plot(mdsE, display="sites", type="text")
# non-metric multidimensional scaling distance (bray curtis) between management types 
mdsB <- metaMDS(bee_diversity_summarized, distance="bray", autotransform=FALSE, trace=0)
plot(mdsB, display="sites", type="text")
# warnings caused by low sample sizes (small number of individuals in each species in each management type)



#### Diversity ############
bee_diversity <- bee_diversity %>%
  select(-"Apis mellifera") # removed honey bees for diversity, should I reconsider?

# View(bee_diversity)
bee_diversity_summarized <- bee_diversity %>%
  group_by(management) %>%
  summarise_at(vars(2:42), sum, na.rm = TRUE) %>%
  select(-management) # View(bee_diversity_summarized)
# 1 = control, 2 = reduced, 3 = agricultural, 4 = semi-natural

#### Species Richness #####
# rank abundance
bee_diversity_sorted <- as.data.frame(apply(bee_diversity_summarized, 1, sort, decreasing=T))
colnames(bee_diversity_sorted) <- c("control park", "reduced park", "agricultural", "semi-natural")
# add rank column
bee_diversity_sorted$rank <- seq.int(nrow(bee_diversity_sorted))
# long format so each community has rank 1:40 with corresponding abundance
bee_diversity_long <- bee_diversity_sorted %>%
  gather(key = "management", value = "abundance",
         1:4)

#plot 
ggplot(bee_diversity_long, aes(x = rank, y = abundance, colour=management)) + 
  geom_line() 
# semi natural top 3 are: Lasioglossum pacatum, and Hylaeus modestus, Melisodes rivalis
# reduced parks top 3 are: Bombus vosnesenskii, and then a tie between: B. impatiens, B. flavifrons,
      # Halictus rubicundus, and Megachile rotundata.
# control parks top 3 are: (all tied at 5) Andrena prunorum, Halictus rubicundus, 
      # Lasioglossum brunneiventre.
# agricultural top 3 are: Agapostemon texanus, Melissodes rivalis, Ceratina acantha

# calculate species richness by management type
tapply(X=bee_diversity_long$abundance, INDEX=bee_diversity_long$management, 
       FUN = function(x) {length(x[x>0])} )
# Species richness increases as the number of individuals in the sample 
# increases. Therefore, care should be taken in comparing the richness of 
# two different samples to make sure that they are sufficiently comparable 
# (here, num. of individuals are NOT comparable across management types so use caution). Rarify?

# Shannon-Weiner diversity
# if there are many individuals of many different species, then it would be very
# difficult to describe in a simple way, and difficult to predict what a sample 
# would look like–disorder, or entropy, would be high.
shannon <- function(n) { 
  n <- n[n>0] # remove zero values
  p <- n/sum(n) # calculate proportion of each species
  -1 * sum( p*log(p) ) # sum (proportions * log proportions) 
}
tapply(X=bee_diversity_long$abundance, INDEX=bee_diversity_long$management, 
       FUN = shannon)

# Simpson dominance (D) = sum(p^2)
# varies between 1 for a single species and declines toward zero with increasing numbers of species
# Simpson diversity index (1 - D)
# i.e. the probability that two randomly selected individuals belong to different species.
Sc <- function(x) {
  p <- x/sum(x)
  1 - sum(p^2)
}
tapply(X=bee_diversity_long$abundance, INDEX=bee_diversity_long$management, 
       FUN = Sc)
# Simpson inverse dominance index (1/D)
# This value could be considered the ‘effective’ number of species, which discounts the rarest species
Si <- function(x) {
  p <- x/sum(x)
  1/sum(p^2)
}
tapply(X=bee_diversity_long$abundance, INDEX=bee_diversity_long$management, 
       FUN = Si)

# Diversity index summaries
dfg <- group_by(bee_diversity_long, management) # group by community
dfg <- select(dfg, -rank)

# calculate indices for each community type.
summarize(dfg,
          richness = length( abundance[abundance>0]),
          H = shannon(abundance),
          Sc = Sc(abundance),
          Si = Si(abundance)
)
# the communities have relatively similar diversity. 
# statistical test to compare?

############ Estimate Total Richness (Species Accum) and Compare Rarified Diversity
N <- colSums(bee_diversity[,2:41]) 
N <- N[N>0]
sum(N>0) # observed species richness for all sites in July/August = 40

estimateR(N) # use abundance-based coverage estimators, ACE and Chao 1, for total sr
# total species richness estimated at ~49 species.

# use a frequency-based estimator, Chao 2, where the data only need to 
# be presence/absence, and for which we also need multiple sample plots. 
# By using multiple plots, we get some idea of of how rare some species 
# are and use that to estimate how many more rare species there might be 
# which are unsampled.
(chaoF <- specpool(bee_diversity[,2:41])) # ~51.5 total species
# We also get a jackknifed estimate. All of these give some estimate 
# of minimum true number of species in an area around our samples.

# estimate species accumulation asymptote using lomolino model
# and plot species accumulation curve
sp1 <- specaccum(bee_diversity[,2:41])
sp2 <- specaccum(bee_diversity[,2:41], "random")
sp2
summary(sp2)
plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp2, col="yellow", add=TRUE, pch="+")
## Fit Lomolino model to the exact accumulation
mod1 <- fitspecaccum(sp1, "lomolino")
# The Lomolino model (SSlomolino) is Asym/(1 + slope^log(xmid/area)) (Lomolino 2000, Dengler 2009). 
# Parameter Asym is the asymptotic maximum number of species, 
# slope is the maximum slope of increase of richness, and 
# xmid is the area where half of the maximum richness is achieved.
coef(mod1) # this should be closer to the frequency based Chao 2 estimator since 
# speaccum method is based on sampling SITES without replacement (not individuals).
fitted(mod1)
plot(sp1)
## Add Lomolino model using argument 'add'
plot(mod1, add = TRUE, col=2, lwd=2)

## Fit Arrhenius model to the exact accumulation
mod2 <- fitspecaccum(sp1, "arrhenius")
# The Arrhenius model (SSarrhenius) is the expression k*area^z. This is the 
# most classical model that can be found in any textbook of ecology (and also 
# in Dengler 2009). Parameter z is the steepness of the species-area curve, 
# and k is the expected number of species in a unit area.
# as x approaches inf, y approach inf (no limit)
coef(mod2)
fitted(mod2)
plot(sp1)
## Add Lomolino model using argument 'add'
plot(mod2, add = TRUE, col=2, lwd=2)


## Fit Arrhenius models to all random accumulations
mods <- fitspecaccum(sp2, "arrh")
plot(mods, col="hotpink")
boxplot(sp2, col = "yellow", border = "blue", lty=1, cex=0.3, add= TRUE)
## Use nls() methods to the list of models
sapply(mods$models, AIC)

##### Rarefied species diversity
# 1 = control, 2 = reduced, 3 = agricultural, 4 = semi-natural
spAbund <- rowSums(bee_diversity_summarized)  #gives the number of individuals found in each plot
spAbund # view observations per plot 

raremin <- min(rowSums(bee_diversity_summarized)) # rarefaction uses the smallest number of observations per sample to 
# extrapolate the expected number if all other samples only had that number of observations
raremin # view smallest # of obs ()
# only one species at the lowest site so it won't really work to compare 
# individual based rarefied div unless minmum species is >2.

sRare <- rarefy(bee_diversity_summarized, raremin) # now use function rarefy
sRare #gives an "expected"rarefied" number of species (not obs) if only 1 individuals were present

rarecurve(bee_diversity_summarized, col = "blue")

##### repeat with parks only
bee_diversity_summarized_parks <- rbind(bee_diversity_summarized[1:2,])
# 1 = control, 2 = reduced
spAbund <- rowSums(bee_diversity_summarized_parks)  #gives the number of individuals found in each plot
spAbund # view observations per plot 

raremin <- min(rowSums(bee_diversity_summarized_parks)) # rarefaction uses the smallest number of observations per sample to 
# extrapolate the expected number if all other samples only had that number of observations
raremin # view smallest # of obs ()
# only one species at the lowest site so it won't really work to compare 
# individual based rarefied div unless minmum species is >2.

sRare <- rarefy(bee_diversity_summarized_parks, raremin) # now use function rarefy
sRare #gives an "expected"rarefied" number of species (not obs) if only 1 individuals were present

rarecurve(bee_diversity_summarized_parks, col = "blue")


############# Distributions
# fit the distributions
N <- rev(sort(colSums(bee_diversity_summarized)))
# get the expected ranks for the log series distribution
# standard exponential integral
sei <- function(t = 1) exp(-t)/t
alpha <- optimal.theta(N)
Nt <- sum(N)
rank.logseries <- sapply(N, function(x) {
  n <- x * log(1 + alpha/Nt )
  f <- integrate(sei, n, Inf)
  fv <- f[["value"]]
  alpha * fv } )

mod <- radfit(N)
par(mar=c(5,4,1,1))
plot(mod) 
lines(rank.logseries, N, lty=2)

