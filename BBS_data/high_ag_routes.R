## checking the highest ag routes from 1992 - what happened?

# packages

require(ggthemes)
require(wesanderson)
require(mclust)
require(ggplot2)
require(vegan)
require(dplyr)
require(data.table)
require(tidyr)


# data

# setwd() ## - just into the NCEAS-RENCI folder (your machine)
setwd("/Users/Liv/Documents/NCEAS_GIT/NCEAS-RENCI_2014/")

high_ag92 <- read.csv("BBS_data/high_ag_RTENO_5000.csv")
high_ag92 <- high_ag92[2:3]

BBS_traits <- read.csv("BBS_data/revised_BBS_data_for_analysis.csv")
BBS_traits$BEHAVIOR <- factor(ifelse(as.character(BBS_traits$BEHAVIOR) == "flycatching", "fly_catching", as.character(BBS_traits$BEHAVIOR)))
head(BBS_traits)
BBS_traits_highag <- BBS_traits %>% filter(RTENO %in% high_ag92$RTENO)%>% select(RTENO, year, sum_route_abundance,  DIET, BEHAVIOR, CONSERVATION, COMMON_NAME)
BBS_traits_highag_wide <- BBS_traits_highag  %>% group_by(RTENO, year, COMMON_NAME, CONSERVATION)%>%summarise(sum_route_abundance = mean(sum_route_abundance)) %>% ungroup() %>% group_by(RTENO, year, CONSERVATION) %>% summarise(sum_route_abundance = sum(sum_route_abundance))

require(gridExtra)
# birds by conservation in high areas
conservation <- ggplot(BBS_traits_highag_wide, aes(x = year, y = sum_route_abundance)) +
  geom_line(aes(group = RTENO), alpha = 0.3) +
  geom_smooth(method = "glm", family = "poisson")+
  facet_wrap(~CONSERVATION, scales = "free")

BBS_traits_highag_diet <- BBS_traits_highag   %>% group_by(RTENO, year, DIET, COMMON_NAME)%>%summarise(sum_route_abundance = mean(sum_route_abundance)) %>% ungroup() %>% group_by(RTENO, year, DIET) %>% summarise(sum_route_abundance = sum(sum_route_abundance))


# birds by diet in high areas
diet <- ggplot(subset(BBS_traits_highag_diet, DIET %in% c("carrion", "insects", "seeds", "omnivore", "fruit")), aes(x = year, y = sum_route_abundance)) +
  geom_line(aes(group = RTENO), alpha = 0.3) +
  geom_smooth(method = "glm", family = "poisson")+
  facet_wrap(~DIET, scales = "free")



BBS_traits_highag_behav <- BBS_traits_highag  %>% group_by(RTENO, year, BEHAVIOR, COMMON_NAME)%>%summarise(sum_route_abundance = mean(sum_route_abundance)) %>% ungroup()%>% group_by(RTENO, year, BEHAVIOR) %>% summarise(sum_route_abundance = sum(sum_route_abundance))
# birds by behaviour in high areas
behavior <- ggplot(subset(BBS_traits_highag_behav, !BEHAVIOR %in% c("", "aerial_dive", "dabbler", "hovering", "probing", "stalking", "surface_dive" )), aes(x = year, y = sum_route_abundance)) +
  geom_line(aes(group = RTENO), alpha = 0.3) +
  geom_smooth(method = "glm", family = "poisson")+
  facet_wrap(~BEHAVIOR, scales = "free")


## all birds on routes

BBS_traits_highag_all <- BBS_traits_highag  %>% group_by(RTENO, year,COMMON_NAME)%>%summarise(sum_route_abundance = mean(sum_route_abundance)) %>% ungroup() %>% group_by(RTENO, year) %>% summarise(sum_route_abundance = sum(sum_route_abundance))

all_birds <- ggplot(BBS_traits_highag_all, aes(x = year, y = sum_route_abundance)) +
  geom_line(aes(group = RTENO), alpha = 0.3) +
  geom_smooth(method = "glm", family = "poisson") +
  ggtitle("All bird abundance")

## species evenness
BBS_traits_highag_species <- BBS_traits %>% filter(RTENO %in% high_ag92$RTENO)%>% select(RTENO, year, sum_route_abundance, COMMON_NAME) %>% ungroup() %>% group_by(RTENO, year, COMMON_NAME) %>% summarise(sum_route_abundance = mean(sum_route_abundance)) %>% spread(key = COMMON_NAME, value = sum_route_abundance, fill = 0 )

metabirds <- metaMDS(BBS_traits_highag_species[-c(1:2)], k = 3, "jaccard")
plot(metabirds)
envbirds <- envfit(metabirds, env = BBS_traits_highag_species$year, strata = BBS_traits_highag_species$RTENO)
ordisurf(metabirds, BBS_traits_highag_species$year)
adonbirds <- adonis(vegdist(BBS_traits_highag_species[-c(1:2)], "jaccard") ~ year, data = BBS_traits_highag_species, strata = "RTENO")
beta_birds <- betadisper(vegdist(BBS_traits_highag_species[-c(1:2)]), group = BBS_traits_highag_species$year, type = "median")
plot(beta_birds)
boxplot(beta_birds)

######### for presences and absences

metabirdsbin <- metaMDS(vegdist(BBS_traits_highag_species[-c(1:2)], "jaccard", binary = TRUE), k = 3)
plot(metabirds)
envbirds <- envfit(metabirdsbin, env = BBS_traits_highag_species$year, strata = BBS_traits_highag_species$RTENO)
ordisurf(metabirdsbin, BBS_traits_highag_species$year)
adonbirdsbin <- adonis(vegdist(BBS_traits_highag_species[-c(1:2)], "jaccard", binary = TRUE) ~ year, data = BBS_traits_highag_species, strata = "RTENO")
beta_birds_bin <- betadisper(vegdist(BBS_traits_highag_species[-c(1:2)], binary = TRUE), group = BBS_traits_highag_species$year, type = "centroid")
plot(beta_birds)
boxplot(beta_birds)


######eveness 
BBS_traits_highag_species2 <- BBS_traits_highag_species %>% mutate(shannon = diversity(BBS_traits_highag_species[-c(1:2)], "shannon")) %>% select(RTENO, year, shannon)

shannon <- ggplot(BBS_traits_highag_species2, aes(x = year, y = shannon)) + geom_line(aes(group = RTENO)) + ggtitle("Shannon (species evenness)")


#
BBS_traits_highag_spciesn <- BBS_traits_highag %>% group_by(year, RTENO) %>% summarise(n_species = length(unique(COMMON_NAME)))

speciesn <- ggplot(BBS_traits_highag_spciesn, aes(x = year, y = n_species)) +
  geom_line(aes(group = RTENO)) + geom_smooth(method = "glm", family = "poisson")

all_graphs <- arrangeGrob(diet,  conservation, speciesn,behavior, shannon, all_birds, ncol = 3)
all_graphs


BBS_traits_highag_frame <- expand.grid(COMMON_NAME = unique(BBS_traits_highag$COMMON_NAME), year = unique(BBS_traits_highag$year), RTENO = unique(BBS_traits_highag$RTENO))

BBS_traits_highag2 <- BBS_traits_highag %>% select(RTENO, year, COMMON_NAME, sum_route_abundance)


BBS_traits_highag_presence <- left_join(BBS_traits_highag_frame, BBS_traits_highag2, by = c("COMMON_NAME", "year", "RTENO"))
BBS_traits_highag_presence[is.na(BBS_traits_highag_presence)] <- 0
BBS_traits_highag_presence <- BBS_traits_highag_presence %>% group_by(RTENO, year, COMMON_NAME) %>%
  summarise(species_counts =  mean(sum_route_abundance),
            species_seen = ifelse(species_counts > 0, 1, 0)) %>% filter(group_by(COMMON_NAME), max(species_counts) > 4)

maxs <- BBS_traits_highag_presence %>%group_by(COMMON_NAME) %>%
  summarise(max_seen = max(species_counts)) %>% filter(max_seen > 4)
BBS_traits_highag_presence <- BBS_traits_highag_presence %>%
  filter(COMMON_NAME %in% maxs$COMMON_NAME)


species_likelihood_seen <- ggplot(BBS_traits_highag_presence, aes(x = year, y = species_seen)) +
 # geom_line(aes(group = RTENO)) + 
  geom_smooth(method = "glm", family = "binomial") +
  facet_wrap(~ COMMON_NAME)
species_likelihood_seen


species_likely_counts <- ggplot(BBS_traits_highag_presence, aes(x = year, y = species_counts)) +
  geom_line(aes(group = RTENO), alpha = 0.5) + 
  geom_smooth(method = "glm", family = "poisson") +
  facet_wrap(~ COMMON_NAME, scales = "free")
species_likely_counts


# not much change

neonics_buffers <- read.csv("Pesticides/pest_buff_overtime_MOA.csv")
neonics_buffers_highag <- neonics_buffers %>% filter(RTENO %in% high_ag92$RTENO & buffer == 5000)
head(neonics_buffers_highag)
neonics_buffers_highag
names(neonics_buffers_highag) <- tolower(names(neonics_buffers_highag))
names(BBS_traits_highag_presence) <- tolower(names(BBS_traits_highag_presence))
BBS_traits_highag_presence <- data.frame(BBS_traits_highag_presence)
neonics_buffers_highag <- data.frame(neonics_buffers_highag)

neonic_birds <- left_join(BBS_traits_highag_presence, neonics_buffers_highag)

head(neonic_birds)
require(lme4)

glmer1 <- glmer(species_seen ~ kg_buff + year + (1|rteno),
                family = "binomial", data = neonic_birds)

summary(glmer1)

glmer2 <-  glmer(species_seen ~ kg_buff + kg_buff:moause + meanbuff_kgkm2ag+ (1|year) + (1|rteno) + (1|common_name),
                 family = "binomial", data = neonic_birds) 

summary(glmer2)

names(neonic_birds)
gbmstep1 <- gbm.step(data = neonic_birds, 
                     gbm.y = 5,
                     gbm.x = c(1, 2, 3, 7, 8, 9, 10),
                     tree.depth = 4, 
                     family = "bernoulli")