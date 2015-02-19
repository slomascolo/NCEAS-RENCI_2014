# require(RCurl)
# BBSurl <- getURL("https://raw.githubusercontent.com/slomascolo/NCEAS-RENCI_2014/master/BBS_data/Ohio_BBS.csv")
# BBS_ohio <- read.csv(text = BBSurl)

require(dplyr)
traits_commmon_steep <- read.csv("/Users/Liv/Documents/NCEAS_GIT/NCEAS-RENCI_2014/BBS_data/commonbirds_steepdecline_traits.csv")

head(traits_commmon_steep)
levels(traits_commmon_steep$common_name)
str(traits_commmon_steep)
traits_commmon_steep_insectohio <- as.data.frame(traits_commmon_steep %>%
  filter(ohio_presence > 0 & diet == "insects"))
str(traits_commmon_steep_insectohio)
traits_commmon_steep_insectohio$common_name <- factor(toupper(traits_commmon_steep_insectohio$common_name))
levels(traits_commmon_steep_insectohio$common_name)

AOU_codes <- read.csv("/Users/Liv/Documents/NCEAS_GIT/NCEAS-RENCI_2014/BBS_data/AOU_codes.csv")
traits_commmon_steep_insectohio2 <- as.data.frame(inner_join(AOU_codes, traits_commmon_steep_insectohio))
head(traits_commmon_steep_insectohio2)


ohio_BBS <- read.csv("/Users/Liv/Documents/NCEAS_GIT/NCEAS-RENCI_2014/BBS_data/fifty7.csv")
head(ohio_BBS)

ohio_BBS_insect <- as.data.frame(inner_join(ohio_BBS, traits_commmon_steep_insectohio2))
ohio_BBS_insect$common_name<-factor(ohio_BBS_insect$common_name)
head(ohio_BBS_insect)

ohio_BBS_routes <- read.csv("/Users/Liv/Documents/NCEAS_GIT/NCEAS-RENCI_2014/BBS_data/BBS_route_lat_lon.csv")
head(ohio_BBS_routes)

ohio_BBS_routes_countiesraw <- read.csv("/Users/Liv/Documents/NCEAS_GIT/NCEAS-RENCI_2014/BBS_data/BBSdata.by.county.csv")
head(ohio_BBS_routes_countiesraw)
ohio_BBS_routes_countiesraw2 <- ohio_BBS_routes_countiesraw[!duplicated(ohio_BBS_routes_countiesraw$Route), c(1,4,5,6,7)]

ohio_BBS_birds <- inner_join(ohio_BBS_insect, ohio_BBS_routes_countiesraw2)
head(ohio_BBS_birds)
names(ohio_BBS_birds)

ohio_BBS_birds$route_abundance <- rowSums(ohio_BBS_birds[grep("^Stop[0-9]+", names(ohio_BBS_birds))])

class(ohio_BBS_birds$year)
names(ohio_BBS_birds)

ohio_BBS_birds_summary <- ohio_BBS_birds %>%
  group_by(year, GEOID, common_name) %>%
  summarise(mean_abund = mean(route_abundance),
            sd = sd(route_abundance),
            n = n()) %>%
  mutate(sem_abund = sd/sqrt(n))
which(ohio_BBS_birds_summary$n>1)

names(ohio_BBS_birds_summary)
ggplot(ohio_BBS_birds_summary, aes(x = YEAR, y = MEAN_ABUND))+
  geom_line(aes(colour = GEOID, group = GEOID))+
  facet_wrap(~COMMON_NAME, scales = "free") +
  theme_bw()

#now replacing w
names(ohio_BBS_birds_summary)<-toupper(names(ohio_BBS_birds_summary))
names(ohio_BBS_birds_summary)
head(ohio_BBS_birds_summary)
class(ohio_BBS_birds_summary$GEOID)
 neonics_facts_ohio$GEOID<-as.numeric(neonics_facts_ohio$GEOID)
# ohio_insects <- inner_join(ohio_BBS_birds_summary, neonics_facts_ohio, by = c("YEAR", "GEOID"))
# ohio_insects <- inner_join(neonics_facts_ohio, ohio_BBS_birds_summary, by = c("YEAR", "GEOID"))
# ohio_insects <- merge(ohio_BBS_birds_summary, neonics_facts_ohio, by = c("YEAR", "GEOID"))
ohio_insects <- merge(neonics_facts_ohio[neonics_facts_ohio$YEAR >= 1996 & neonics_facts_ohio$YEAR <=2009,], ohio_BBS_birds_summary[ohio_BBS_birds_summary$YEAR >= 1996 & ohio_BBS_birds_summary$YEAR <=2009,], by = c("YEAR", "GEOID"))

ggplot(ohio_BBS_birds_summary, aes(x = MEAN_ABUND))+
  geom_histogram()+
  theme_bw()+
  facet_wrap(~COMMON_NAME, scales = "free")

names(ohio_insects)
ggplot(subset(ohio_insects,ohio_insects$YEAR %in% c(2004,2007, 2009) & !ohio_insects$COMMON_NAME %in% c("BLACK TERN", "BLACKPOLL WARBLER")), aes(y = log(MEAN_ABUND), x = log(kg_density)))+
  geom_point(aes(colour = factor(YEAR)))+
  scale_colour_brewer(type = "qual", palette = "Dark2")+
  geom_smooth(aes(colour = factor(YEAR)), method = lm)+
  theme_bw()+
  facet_grid(COMPOUND~COMMON_NAME, scales = "free")+
  coord_fixed()

ggplot(subset(ohio_insects, !ohio_insects$COMMON_NAME %in% c("BLACK TERN", "BLACKPOLL WARBLER")), aes(y = log(MEAN_ABUND), x = YEAR))+
  geom_point(aes(colour = factor(COMMON_NAME)))+
  scale_colour_brewer(type = "qual", palette = "Dark2", guide = FALSE)+
  geom_smooth(aes(colour = factor(COMMON_NAME)), method = lm)+
  theme_bw()+
  facet_grid(COMMON_NAME~COMPOUND, scales = "free")+
  coord_fixed()


require(lme4)
names(ohio_insects_wide)
ohio_insects_wide <- spread(ohio_insects, key = COMPOUND, value = kg_density) 
tail(ohio_insects_wide)
class(ohio_insects_wide$GEOID)
ohio_insects_wide$GEOID<-as.factor(ohio_insects_wide$GEOID)
ohio_insects_wide$county<-as.factor(ohio_insects_wide$county)
ohio_insects$MEAN_ABUND <- round(ohio_insects$MEAN_ABUND)
ohio_insects_wide$MEAN_ABUND <- round(ohio_insects_wide$MEAN_ABUND)

mod1 <- glm(data=ohio_insects_wide, MEAN_ABUND ~ COMMON_NAME * CLOTHIANIDIN * ACETAMIPRID *IMIDACLOPRID*THIAMETHOXAM + county,
              family = poisson)
summary(mod1)

### i think i need to go back and put in wide levels of compound and put zero where not recorded######
mod1_pred <- predict(mod1, type = "response")

mod2<- glm(data= ohio_insects[ohio_insects$COMMON_NAME == "CHIMNEY SWIFT",], MEAN_ABUND ~ kg_density:COMPOUND,
           family = "poisson")
summary(mod2)
