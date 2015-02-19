
setwd("/Users/Liv/ebd_US_relMay-2014/raw_pesticides/")
filenames <- list.files()
pesticides_raw <- do.call("rbind", lapply(filenames[grep("high", filenames)], read.table, header = TRUE, sep = "\t"))
pesticides_raw_ohio <- pesticides_raw[which(pesticides_raw$STATE_FIPS_CODE == 39), ]
rm(pesticides_raw)

names(pesticides_raw_ohio)
neonics_raw_ohio <- pesticides_raw_ohio %>% 
  filter(COMPOUND %in% c("ACETAMIPRID", "CLOTHIANIDIN", "IMIDACLOPRID", "THIAMETHOXAM"))

head(neonics_raw_ohio)
ggplot(neonics_raw_ohio, aes(x = YEAR, y = KG))+
  geom_point()+
  facet_wrap(~COMPOUND)

ohio_facts<-read.csv("/Users/Liv/Documents/NCEAS_GIT/NCEAS-RENCI_2014/BBS_data/DEC_10_SF1_GCTPH1/DEC_10_SF1_GCTPH1.ST05_with_ann.csv")
#we want ohio facts [,]
head(ohio_facts)
names(ohio_facts)
ohio_facts_r <- ohio_facts[ , c(2, 3, 5, 7, 10:14)]
head(ohio_facts_r)
names(ohio_facts_r) <- c("state_fips", "state", "GEOID", "county",
                         "total_area", "total_water", "total_land",
                         "popdens_sqmile", "housedens_sqmile")
require(stringr)
neonics_raw_ohio$county_id<-str_pad(neonics_raw_ohio$COUNTY_FIPS_CODE, 3, "left", pad = "0")
neonics_raw_ohio$GEOID <- paste(neonics_raw_ohio$STATE_FIPS_CODE, neonics_raw_ohio$county_id, sep = "")


ohio_facts_r <- ohio_facts_r[-c(1,2),]
str(ohio_facts_r)
str(neonics_raw_ohio)

neonics_facts_ohio <- merge(neonics_raw_ohio, ohio_facts_r)

class(neonics_facts_ohio$total_land)
range(neonics_facts_ohio$YEAR)
head(neonics_facts_ohio)
neonics_facts_ohio$total_land<-as.numeric(neonics_facts_ohio$total_land)
neonics_facts_ohio$kg_density <- neonics_facts_ohio$KG/neonics_facts_ohio$total_land
ggplot(neonics_facts_ohio, aes(x = YEAR, y = kg_density)) +
         geom_point()+
         facet_wrap(~COMPOUND)+
         theme_bw()
require(lme4)


mod1 <- glmer(data  )


merge
levels(as.factor(pesticides_raw_ohio$YEAR))
as.factor(pesticides_raw_ohio$COMPOUND)
levels(pesticides_raw_ohio$COMPOUND)
head(pesticides_raw_ohio,50)
test<-pesticides_raw_ohio[order(pesticides_raw_ohio$KG,pesticides_raw_ohio$COMPOUND), ]
unique(test$COMPOUND)
dd[ order(x, -y, z), ]
names(pesticides_raw_ohio)
test_pest<-pesticides_raw_ohio_years %>% 
  group_by(COMPOUND, YEAR) %>%
  summarise(sum_KG = sum(KG)) %>%
  filter(sum_KG > 1000)
test_pest
unique(test_pest$COMPOUND)

require(dplyr)
pesticides_raw_ohio_insectic <- data.frame(filter(pesticides_raw_ohio, COMPOUND %in% c("PROPARGITE", "PHORATE", "METHOMYL", "METHYL PARATHION", "ETHOPROPHOS", "FONOFOS", "CARBOFURAN")))
head(pesticides_raw_ohio_insectic)
ggplot(pesticides_raw_ohio_insectic, aes(x = YEAR, y = KG, colour = COMPOUND))+
  geom_point()+
  geom_smooth(aes(group = COMPOUND), method = lm)+
  theme_bw()+
  facet_wrap(~COMPOUND)

ggplot(pesticides_raw_ohio_insectic, aes(x = KG, colour = COMPOUND))+
  geom_density()+
  theme_bw()+
  facet_wrap(~COMPOUND, scales = "free")

pesticides_raw_ohio_insectic$COUNTY_FIPS_CODE<-factor(pesticides_raw_ohio_insectic$COUNTY_FIPS_CODE)
ggplot(pesticides_raw_ohio_insectic, aes(x =YEAR, y = KG))+
  scale_y_continuous()+
#  geom_point()+
#  geom_pointrange(stat = "sum", aes())+
  theme_bw()+
  facet_wrap(~COUNTY_FIPS_CODE)

ggplot(pesticides_raw_ohio_insectic, aes(x =YEAR, y = KG))+
  scale_y_continuous()+
  #  geom_point()+
  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange")+
  theme_bw()+
  facet_wrap(~COUNTY_FIPS_CODE)


names(pesticides_raw_ohio_insectic)
pesticides_raw_ohio_herbs <- data.frame(filter(pesticides_raw_ohio, COMPOUND %in% c("GLYPHOSATE", "ATRAZINE", "ACETOCHLOR", "TRIFLURALIN", "PROPANIL", "ALACHLOR", "EPTC")))

ggplot(pesticides_raw_ohio_herbs, aes(x = KG, colour = COMPOUND))+
  geom_density()+
  theme_bw()+
  facet_wrap(~COMPOUND, scales = "free")

ggplot(pesticides_raw_ohio_herbs, aes(x = YEAR, y = KG, colour = COMPOUND))+
  geom_point()+
  geom_smooth(aes(group = COMPOUND))+
  theme_bw()+
  facet_wrap(~COMPOUND)

pesticides_raw_ohio_insectic$COMPOUND <-factor(pesticides_raw_ohio_insectic$COMPOUND)
levels(pesticides_raw_ohio_insectic$COMPOUND)

pesticides_raw_ohio_years <- pesticides_raw_ohio[which(pesticides_raw_ohio$YEAR %in% c("1999", "2004", "2009")), ]

pesticides_raw_ohio_years$YEAR<-factor(pesticides_raw_ohio_years$YEAR)
pesticides_raw_ohio_years$pesticide_type <- "insecticides"
head(pesticides_raw_ohio_years)

testing2 <- pesticides_raw_ohio_years %>% 
  group_by(COMPOUND) %>%
  filter(KG > 0 )
length(unique(testing2$COMPOUND))
testing2$COMPOUND


levels(pesticides_raw_ohio_years$YEAR)
install.packages("ggplot2")
require(ggplot2)
names(pesticides_raw_ohio)
ggplot(pesticides_raw_ohio, aes(x = YEAR, y = KG, colour = COUNTY_FIPS_CODE))+
  geom_point()+
  geom_line(aes(group = COUNTY_FIPS_CODE))+
  facet_wrap(~COMPOUND)
