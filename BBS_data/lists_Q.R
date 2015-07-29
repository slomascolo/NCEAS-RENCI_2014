# 21 May 2015 
## WHY DOES IT ONLY SAVE ME THE LAST ONE?? WOE IS ME.

# package
if(!require(vegan)){
  install.packages("vegan", repos = 'http://cran.stat.auckland.ac.nz/')
  library(vegan)
}

# data
load("lists98.Rdata")

dt = 1
# problematic loop
metas98 <- list()
for (dt in seq(along = lists98)){
  meta_n = metaMDS(lists98[[dt]][3:9])
  meta_names <- paste("buffer", names(lists98), sep = "_")
  metas98[[dt]] = meta_n
  par(mfrow = c(2,3))
  for (n in seq(along = metas98)){
    plot(metas98[[n]], main = paste("Plot of 1998 with buffer of ", names(lists98)[n], " metres", sep = ""))
    text(metas98[[n]], display = "species")
    ordisurf(metas98[[n]], lists98[[n]]$neonic, add = T, col = n)
  }
}

for (dt in seq(along = lists98)){
  meta_n = metaMDS(lists98[[dt]][3:9])
  meta_names <- paste("buffer", names(lists98), sep = "_")
  metas98[[dt]] = meta_n
  par(mfrow = c(2,3))
  dt = list()
  for (n in seq(along = metas98)){
    plot(metas98[[n]], main = paste("Plot of 1998 with buffer of ", names(lists98)[n], " metres", sep = ""))
    text(metas98[[n]], display = "species")
    ordisurf(metas98[[n]], lists98[[n]]$neonic, add = T, col = n)
    metaj <- data.frame(buffer = names(lists98)[n], 
                        RTENO = lists98[[n]]$RTENO,
                        NMDS1 = scores(metas98[[n]])[ , 1],
                        NMDS2 = scores(metas98[[n]])[ , 2])
    dt[[n]] <- metaj
  }
  dt2 <- do.call(rbind,dt)
}

dt2$RTENO <- factor(dt2$RTENO)
ggplot(dt2, aes(x = NMDS1, y = NMDS2))+
  geom_point(aes(colour = RTENO, size = buffer)) +
  geom_line(aes(group = RTENO))
 
ab<-procrustes(meta1, meta2)
bc<-procrustes(meta2, meta3)
str(ab)



par(mfrow = c(2,3))
for (n in seq(along = metas98)){
plot(metas98[[n]], main = paste("Plot of 1998 with buffer of ", names(lists98)[n], " metres", sep = ""))
text(metas98[[n]], display = "species")
ordisurf(metas98[[n]], lists98[[n]]$neonic, add = T, col = n)
}

plot(metas98[[1]])
text(metas98[[1]], display = "species")
ordisurf(x = metas98[[1]], y = lists98[[1]]$neonic)

metas98<- list(meta1, meta2, meta3, meta4, meta5, meta6)
plot(metas98[[3]])
summary(metas98[[1]])
meta1 <- metaMDS(lists98[[1]][3:9])
meta2 <- metaMDS(lists98[[2]][3:9])
meta3 <- metaMDS(lists98[[3]][3:9])
meta4 <- metaMDS(lists98[[4]][3:9])
meta5 <- metaMDS(lists98[[5]][3:9])
meta6 <- metaMDS(lists98[[6]][3:9])
metas98[[1]] <- metaMDS(lists98[[1]][3:9])
metas98[[2]] <- metaMDS(lists98[[2]][3:9])
metas98[[3]] <- metaMDS(lists98[[3]][3:9])



for (dt in seq(along = lists98)){
  metas98 = list()
  metas98[[dt]] <- append(metas98,metaMDS(lists98[[dt]][3:9]))
  names(metas98)[[dt]] = paste("buffer_", names(lists98)[dt], sep = "")
}
str(metas98)


######## birds  #####

head(BBS_ln)

### routes #####
BBS_birds2 <- BBS_ln %>% 
  ungroup() %>%
  filter(YEAR %in% c(1998, 2011)) %>%
  group_by(YEAR, RTENO, COMMON_NAME) %>%
  summarise(mean_abundance = mean(SUM_ROUTE_ABUNDANCE)) %>%
  ungroup() %>%
  spread(COMMON_NAME, mean_abundance)
head(BBS_birds2)

BBS_birds2_98 <- subset(BBS_birds2, YEAR == 1998)
BBS_birds2_98$RTENO <- factor(BBS_birds2_98$RTENO)

BBS_birds2_11 <- subset(BBS_birds2, YEAR == 2011)
BBS_birds2_11$RTENO <- factor(BBS_birds2_11$RTENO)

BBS_birds2_98p <- BBS_birds2_98[BBS_birds2_98$RTENO  %in% BBS_birds2_11$RTENO,]

BBS_birds2_11p <-BBS_birds2_11[BBS_birds2_11$RTENO %in% BBS_birds2_98p$RTENO,]
BBS_birds2_98p$RTENO %in% BBS_birds2_11p$RTENO
BBS_birds2_11p$RTENO %in% BBS_birds2_98p$RTENO
routes <- BBS_birds2_11p$RTENO



# meta acutal ####
baser <- expand.grid(YEAR = c(1998, 2011),
                     RTENO = as.integer(as.character(routes)),
                     COMMON_NAME = unique(BBS_ln$COMMON_NAME))

BBS_birds <- BBS_ln %>% 
  filter(YEAR %in% c(1998, 2011) & RTENO %in% routes) %>%
  group_by(YEAR, RTENO, COMMON_NAME) %>%
  summarise(mean_abundance = mean(SUM_ROUTE_ABUNDANCE)) %>%
  left_join(baser, .) %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance)) %>%
  spread(COMMON_NAME, mean_abundance)
head(BBS_birds)


BBS_birds98 <- filter(BBS_birds, YEAR == 1998)
BBS_birds11 <- filter(BBS_birds, YEAR == 2011)

birds_meta98 <- metaMDS(BBS_birds98[-c(1:2)])
birds_meta11 <- metaMDS(BBS_birds11[-c(1:2)])

basertraits <- expand.grid(YEAR = c(1998, 2011),
                     RTENO = as.integer(as.character(routes)),
                     DIET = c("birds"    ,     "carrion",       "fish"     ,     "fruit"     ,    "insects"   ,    "mammals"    ,   "nectar"    ,   
                             "omnivore"  ,    "plants"    ,    "seeds"     ,    "small_animals")) 

BBS_birdstraits <- BBS_ln %>% 
  filter(YEAR %in% c(1998, 2011) & RTENO %in% routes) %>%
  ungroup() %>%
  group_by(YEAR, RTENO, DIET) %>%
  summarise(mean_abundance = mean(SUM_ROUTE_ABUNDANCE)) %>%
  left_join(basertraits, .) %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance)) %>%
  spread(DIET, mean_abundance)
head(BBS_birdstraits)

names(BBS_ln)
levels(BBS_ln$NEONIC)
BBS_ln$NEONIC <- factor(BBS_ln$NEONIC)
BBS_env <- BBS_ln %>% filter(YEAR %in% c(1998, 2011) & RTENO %in% routes) %>% select(YEAR, RTENO, buffer, NEONIC, SUM_high_kg, agriculture, barren, developed, forest, grassland_shrub, water, wetlands) %>% group_by(YEAR, RTENO, buffer, NEONIC) %>%slice(1)%>% ungroup() %>% spread(NEONIC, SUM_high_kg) %>% inner_join(BBS_birdstraits, ., by = c("YEAR", "RTENO")) %>% filter(!RTENO == 66035)
head(BBS_env)

BBS_env$RTENO <- factor(BBS_env$RTENO)

names(BBS_birds98traits)
BBS_birds98traits <- filter(BBS_env, YEAR == 1998)
BBS_birds11traits <- filter(BBS_env, YEAR == 2011)
# BBS_birds98traits$check <- with(BBS_birds98traits, interaction(RTENO, buffer))
# BBS_birds11traits$check <- with(BBS_birds11traits, interaction(RTENO, buffer))
# BBS_birds98traits$check %in% BBS_birds11traits$check
# BBS_birds11traits$check %in% BBS_birds98traits$check
# BBS_birds98traits[1:7,]
# BBS_birds11traits[1:7,]

birds_meta98traits <- metaMDS(BBS_birds98traits[BBS_birds98traits$buffer == 200,-c(1:2, 14:23)]) 
birds_meta11traits<- metaMDS(BBS_birds11traits[BBS_birds11traits$buffer == 200, -c(1:2, 14:23)])

birds_metatraits <- metaMDS(BBS_env[BBS_env$buffer == 200, -c(1:2, 14:23)])
points(birds_metatraits, col = BBS_env[BBS_env$buffer == 200, "YEAR"])
ordiellipse(birds_metatraits, BBS_env[BBS_env$buffer == 200, "YEAR"])
ordisurf(birds_metatraits, BBS_env[BBS_env$buffer == 10000, "forest"])

plot(birds_meta98traits)
ordisurf(birds_meta98traits, BBS_birds98traits[BBS_birds98traits$buffer == 200, "developed"])
plot(birds_meta11traits)
text(birds_meta11traits, display = "species", col = "darkblue")
ordisurf(birds_meta11traits, BBS_env[BBS_env$buffer == 5000 & BBS_env$YEAR == 2011, "neonic"], add = T)
ordisurf(birds_meta11traits, BBS_env[BBS_env$buffer == 5000 & BBS_env$YEAR == 2011, "agriculture"], add = T, col = "cyan")

plot(procrustes(birds_meta98traits, birds_meta11traits))
text(birds_meta98traits, display = "species", col = "darkgreen")
text(birds_meta11traits, display = "species", col = "cyan")
