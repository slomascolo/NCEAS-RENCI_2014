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

LC_routes_bbs <- read.csv("Landcover/LC_buffers_overtime.csv")

head(LC_routes_bbs)

#convert reclass to vegtype
reclass_table <- data.frame("reclass" = 0:7, "land_cover" = c("nodata", "water", "developed", "barren", "forest", "grassland_shrub", "agriculture", "wetlands"))

#merge conversion table with landcover
landuse_buffers <- merge(LC_routes_bbs, reclass_table, by = "reclass")

head(landuse_buffers) #have a look

landuse_buffers_5000 <- subset(landuse_buffers, buffer == 5000 & YEAR %in% c(1992, 2009))  
# just the 5000 buffer for 92

landuse_buffers_5000_1992 <- landuse_buffers_5000 %>% filter(YEAR ==1992) %>% select(RTENO, km2, land_cover) %>% 
  mutate(km2_abs = abs(km2)) %>% select(RTENO, km2_abs, land_cover) %>%  # this is required as some values neg?
  spread(land_cover, km2_abs) %>% #make data wide for nmds
  ungroup() %>% group_by(RTENO) %>% 
  mutate(rowtotal = agriculture+ barren+ developed+   forest+ grassland_shrub +  water+ wetlands) %>%
  mutate(agriculture_prop = agriculture/rowtotal, 
         barren_prop = barren/rowtotal, 
         developed_prop = developed/rowtotal, 
         forest_prop = forest/rowtotal, 
         grassland_shrub_prop = grassland_shrub/rowtotal,
         water_prop = water/rowtotal, 
         wetlands_prop = wetlands/rowtotal) %>% ungroup()  #there's quicker ways of doing this
head(landuse_buffers_5000_1992)
range(rowSums(landuse_buffers_5000_1992[2:8])) #hu

head(landuse_buffers_5000_1992[, -1])

landuse_buffers_5000_nmds_prop <- landuse_buffers_5000_1992[ , c("agriculture_prop", "barren_prop", 
                                                            "developed_prop", "forest_prop",
                                                            "grassland_shrub_prop", "water_prop", 
                                                            "wetlands_prop")]

landuse_buffers_5000_nmds <- landuse_buffers_5000_1992[ , c("agriculture", "barren", "developed", 
                                                       "forest","grassland_shrub", 
                                                       "water", "wetlands")]

nmds_veg <- metaMDS(landuse_buffers_5000_nmds_prop, 'jaccard', k = 3) #using jaccard instaead of default
# using 3 dimensions = could use 2.  For us, NMDS1 v 2 and NMDS1 v 3 both split data niclely along ag gradient
nmds_veg

#base plot
plot(nmds_veg)
text(nmds_veg, "species")

#extract scores for plotting
landuse_buffers_5000_1992$NMDS1 <- scores(nmds_veg)[,1]
landuse_buffers_5000_1992$NMDS2 <- scores(nmds_veg)[,2]
try(landuse_buffers_5000_1992$NMDS3 <- scores(nmds_veg)[,3])


clust_veg <- Mclust(landuse_buffers_5000_nmds)
#plot(clust_veg)
landuse_buffers_5000_1992$classification <- as.factor(clust_veg$classification)

ordisurf(nmds_veg, landuse_buffers_5000_1992$forest_prop, col = "forestgreen")
ordisurf(nmds_veg, landuse_buffers_5000_1992$agriculture_prop, col = "purple3")
ordisurf(nmds_veg, landuse_buffers_5000_1992$water_prop, col = "darkblue")

ggplot(landuse_buffers_5000_1992, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(colour = classification), size =3)+
scale_colour_brewer(palette = "Dark2")+
  theme_classic()

ggplot(landuse_buffers_5000_1992, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(colour = cut(agriculture_prop,3)), size =3)+
scale_colour_manual(values = wes_palette("GrandBudapest"))+
  #scale_colour_gradient(high = "purple3", low = "orange")+
  theme_classic()

ggplot(landuse_buffers_5000_1992, aes(x = NMDS1, y = NMDS2)) +
  geom_point(data = landuse_buffers_5000_1992 %>% filter(agriculture_prop > 0.8 ), 
             size=3, aes(colour = "High ag"))+
  geom_point(data = landuse_buffers_5000_1992 %>% filter(agriculture_prop < 0.8 & agriculture_prop > 0.5), 
             size=3, aes(colour = "Mod ag"))+
  geom_point(data = landuse_buffers_5000_1992 %>% filter(agriculture_prop <= 0.5), 
             size=3, aes(colour = "Low ag"))+
  #geom_point(aes(colour = cut(agriculture_prop,3)), size =3)+
 # geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = landuse), size = 6)+
  scale_colour_manual(values = wes_palette("GrandBudapest"))+
  #scale_colour_gradient(high = "purple3", low = "orange")+
  theme_classic()

# RTENOS of high ag routes:
high_ag_RTENO <- landuse_buffers_5000_1992 %>% filter(agriculture_prop > 0.5) %>% select(RTENO, agriculture_prop)

#write.csv(high_ag_RTENO, file = "BBS_data/high_ag_RTENO_5000.csv")


### for the 2009 data

landuse_buffers_5000_2009 <- landuse_buffers_5000 %>% filter(YEAR ==2009) %>% select(RTENO, km2, land_cover) %>% 
  mutate(km2_abs = abs(km2)) %>% select(RTENO, km2_abs, land_cover) %>%  # this is required as some values neg?
  spread(land_cover, km2_abs) %>% #make data wide for nmds
  ungroup() %>% group_by(RTENO) %>% 
  mutate(rowtotal = agriculture+ barren+ developed+   forest+ grassland_shrub +  water+ wetlands) %>%
  mutate(agriculture_prop = agriculture/rowtotal, 
         barren_prop = barren/rowtotal, 
         developed_prop = developed/rowtotal, 
         forest_prop = forest/rowtotal, 
         grassland_shrub_prop = grassland_shrub/rowtotal,
         water_prop = water/rowtotal, 
         wetlands_prop = wetlands/rowtotal) %>% ungroup()  #there's quicker ways of doing this
head(landuse_buffers_5000_2009)


landuse_buffers_5000_2009 <- subset(landuse_buffers_5000_2009, RTENO %in% high_ag_RTENO$RTENO) %>% select(RTENO,  agriculture_prop, barren_prop, developed_prop, forest_prop, grassland_shrub_prop, water_prop, wetlands_prop) %>% mutate(YEAR = 2009)
landuse_buffers_5000_1992 <- landuse_buffers_5000_1992 %>% filter(agriculture_prop > 0.5 & RTENO %in% landuse_buffers_5000_2009$RTENO ) %>% select(RTENO,  agriculture_prop, barren_prop, developed_prop, forest_prop, grassland_shrub_prop, water_prop, wetlands_prop) %>% mutate(YEAR = 1992)

high_ag_17 <- rbind(landuse_buffers_5000_2009, landuse_buffers_5000_1992) %>% gather(landusetype, proportion, agriculture_prop:wetlands_prop)

ggplot(high_ag_17, aes(x = YEAR, y = proportion)) +
  geom_line(aes(colour = as.factor(RTENO), group = RTENO)) +
  facet_wrap(~ landusetype, scale = "free") +
  theme_bw()

high_ag_17_nmds <- rbind(landuse_buffers_5000_2009, landuse_buffers_5000_1992)
names(high_ag_17_nmds)
nmds_veg_years <- metaMDS(high_ag_17_nmds[ , 2:8], 'jaccard', k = 2) #using jaccard instaead of default
# using 3 dimensions = could use 2.  For us, NMDS1 v 2 and NMDS1 v 3 both split data niclely along ag gradient
nmds_veg
adonis(vegdist(high_ag_17_nmds[ , 2:8], "jaccard") ~ YEAR, strata =  "RTENO", data = high_ag_17_nmds)

#base plot
plot(nmds_veg)
text(nmds_veg, "species")

species_scores <- data.frame(scores(nmds_veg_years, "species")); species_scores$vegtype <- row.names(species_scores)
#extract scores for plotting
high_ag_17_nmds$NMDS1 <- scores(nmds_veg_years)[,1]
high_ag_17_nmds$NMDS2 <- scores(nmds_veg_years)[,2]

require(grid)

head(high_ag_17_nmds)
high_ag_17_nmds_wide_NMDS1 <-  high_ag_17_nmds%>% select(RTENO, NMDS1, YEAR) %>% spread(YEAR, NMDS1)
names(high_ag_17_nmds_wide_NMDS1) <- c("RTENO", "NMDS1", "NMDS1end")
high_ag_17_nmds_wide_NMDS2 <-  high_ag_17_nmds%>% select(RTENO, NMDS2, YEAR) %>% spread(YEAR, NMDS2)
names(high_ag_17_nmds_wide_NMDS2) <- c("RTENO", "NMDS2", "NMDS2end")

arrow_data <- merge(high_ag_17_nmds_wide_NMDS1, high_ag_17_nmds_wide_NMDS2)

annotation <- data.frame(NMDS1 = 0.4, NMDS2 = .75, label = "Arrows begin in 1992 and point to 2009")

ag_changes <- ggplot(high_ag_17_nmds, aes( x= NMDS1, y = NMDS2)) +
  #geom_point(aes(colour = factor(YEAR))) +
  geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = vegtype), size = 6,
            colour = "grey")+
  theme_bw() +
  geom_segment(data = arrow_data,
               aes(x = NMDS1, xend = NMDS1end, y = NMDS2, yend = NMDS2end),
               arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
  coord_fixed() +
  geom_text(data = annotation, aes(x = NMDS1, y = NMDS2, label = label), 
           colour = "black", fontface = "italic")+
  ggtitle("Changes in landuse (buffer: 5000) around BBS routes \n which had > 50% agriculture in 1992")
ag_changes

ggsave(plot = ag_changes, file = "nmds_ag_changes.pdf", width = 210, height = 148.5, units = "mm")
