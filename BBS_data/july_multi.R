## Birds clustering routes
require(dplyr)
require(mclust)

# to merge AOU codes, BBS data and traits: 
source("sourcing_script_birds_traits_AOU.R")

# now:

head(BBS_traits)
names(BBS_traits)

length(unique(BBS_traits$year))
sort(unique(BBS_traits$year))

route_numbers <- BBS_traits %>% filter(year == 1997) %>%group_by(Route) %>% summarise(n_records = n()) %>%
  arrange(n_records)
route_numbers

route_numbers2 <- BBS_traits %>% filter(year == 2011) %>%group_by(Route) %>% summarise(n_records = n()) %>%
  arrange(n_records)
route_numbers2

route_numbers2_in1 <- route_numbers2 %>% filter(Route %in% route_numbers$Route) route_numbers_in2 <- route_numbers %>% filter(Route %in% route_numbers2_in1$Route)

class(BBS_traits$DIET)
meta_1997 <- BBS_traits %>% filter(year == 1997) %>% 
  select(COMMON_NAME, Route, sum_route_abundance, DIET) %>%
  mutate(diet = ifelse(as.character(DIET) == "", "unknown", as.character(DIET))) %>%
  filter(!diet == "unknown") %>%
  group_by(COMMON_NAME, diet, Route) %>%
  summarise(mean_route_abundance = mean(sum_route_abundance)) %>%
  ungroup() %>%
  group_by(Route, diet) %>%
  summarise(sum_trait_abundance = sum(mean_route_abundance))
head(meta_1997)
meta_1997$diet <- factor(meta_1997$diet)
levels(meta_1997$diet)

meta_19972011 <- BBS_traits %>% filter(year %in% c(1997, 2011)) %>% 
  select(COMMON_NAME, Route, sum_route_abundance, DIET, year) %>%
  mutate(diet = ifelse(as.character(DIET) == "", "unknown", as.character(DIET))) %>%
  filter(!diet == "unknown") %>%
  group_by(COMMON_NAME, diet, Route, year) %>%
  summarise(mean_route_abundance = mean(sum_route_abundance)) %>%
  ungroup() %>%
  group_by(Route, diet, year) %>%
  summarise(sum_trait_abundance = sum(mean_route_abundance))
head(meta_19972011)
meta_19972011$diet <- factor(meta_19972011$diet)
levels(meta_2011$diet)


meta_1997_frame <- expand.grid(diet = unique(meta_19972011$diet),
                               Route = unique(meta_19972011$Route))
meta_1997_expanded <- left_join(meta_1997_frame, meta_19972011)
head(meta_1997_expanded)
meta_1997_expanded[is.na(meta_1997_expanded)] <- 0
meta_1997_spread <- meta_1997_expanded %>% ungroup() %>% filter(year == 1997) %>%select(Route, diet, sum_trait_abundance) %>% spread(diet, sum_trait_abundance)
meta_1997_spread$year <- 1997
meta_2011_spread<- meta_1997_expanded %>% ungroup() %>% filter(year == 2011) %>%select(Route, diet, sum_trait_abundance) %>% spread(diet, sum_trait_abundance)
meta_2011_spread$year <- 2011
meta_97211_spread <- rbind(meta_1997_spread, meta_2011_spread)
meta_97211_spread[is.na(meta_97211_spread)] <- 0

### species not using #####
meta_1997_species <- BBS_traits %>% filter(year == 1997) %>% 
  select(COMMON_NAME, Route, sum_route_abundance) %>%
  group_by(COMMON_NAME, Route) %>%
  summarise(mean_route_abundance = mean(sum_route_abundance))
head(meta_1997_species)

meta_1997_frame_species <- expand.grid(COMMON_NAME = unique(meta_1997_species$COMMON_NAME),
                               Route = unique(meta_1997_species$Route))
meta_1997_expanded_species <- left_join(meta_1997_frame_species, meta_1997_species)
head(meta_1997_expanded)
meta_1997_expanded_species[is.na(meta_1997_expanded_species)] <- 0
meta_1997_spread_species <- spread(meta_1997_expanded_species, COMMON_NAME, mean_route_abundance)
head(meta_1997_expanded_species)

### NMDS #####

## classification here:
names(meta_97211_spread)
ord97 <- metaMDS(meta_97211_spread[,-c(1, 13)])
plot(scores(ord97))
text(ord97, "species")
ordiellipse(ord97, meta_97211_spread$year)
adonis(vegdist(meta_97211_spread[,-c(1, 13)]) ~ year, strata = "Route", data = meta_97211_spread)



head(BBS_traits)
meta_19972011 <- BBS_traits %>% filter(year > 1997 & year < 2011) %>% 
  select(COMMON_NAME, Route, sum_route_abundance, DIET, year) %>%
  mutate(diet = ifelse(as.character(DIET) == "", "unknown", as.character(DIET))) %>%
  filter(!diet == "unknown") %>%
  group_by(COMMON_NAME, diet, Route, year) %>%
  summarise(mean_route_abundance = mean(sum_route_abundance)) %>%
  ungroup() %>%
  group_by(Route, diet, year) %>%
  summarise(sum_trait_abundance = sum(mean_route_abundance))
head(meta_19972011)

route_totals_traits <- meta_19972011 %>% group_by(Route, year) %>% summarise(route_total = sum(sum_trait_abundance)) 




meta_19972011props <- meta_19972011 %>% inner_join(.,route_totals_traits) %>% mutate(proportion_trait = sum_trait_abundance/route_total) %>% group_by(diet, year) %>% summarise(trait_proportion = mean(proportion_trait))

ggplot(meta_19972011props, aes(x = year, y = trait_proportion)) +
  geom_line() +
  facet_wrap(~diet, scales = "free") +
  geom_smooth()
