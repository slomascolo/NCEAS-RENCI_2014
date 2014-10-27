### Just load this from the git:, it's in the NCEAS-RENCI_2014 folde
load(file = "/Users/Liv/Documents/NCEAS_GIT/NCEAS-RENCI_2014/models.Rdata")

require(stringr)

## Response: butterflies, data #####
butterfly.diversity <- read.csv("/Users/Liv/Documents/NCEAS_GIT/NCEAS-RENCI_2014/Bflydata.by.county.csv", header = T)
head(butterfly.diversity)
butterfly.diversity$geoid<-butterfly.diversity[,5]
head(butterfly.diversity)
View(butterfly.diversity)
length(unique(butterfly.diversity$geoid))


## Response: birds
bird.diversity.BBS <- read.csv(file = "/Users/Liv/Documents/NCEAS_GIT/NCEAS-RENCI_2014/BBSdata.by.county.csv", header = T)
head(bird.diversity.BBS)
levels(as.factor(bird.diversity.BBS$Year))
bird.diversity.BBS$geoid <- bird.diversity.BBS[,6]

ggplot(bird.diversity.BBS, aes(x = Year, y = Shannon))+
  geom_point()+
  geom_smooth() +
  theme_bw()


## Pesticides
# Insecticides
setwd("/Users/Liv/ebd_US_relMay-2014/raw_pesticides/")
filenames <- list.files()
pesticides_raw <- do.call("rbind", lapply(filenames[grep("high", filenames)], read.table, header = TRUE, sep = "\t"))
pesticides_raw_ohio <- pesticides_raw[which(pesticides_raw$STATE_FIPS_CODE == 39), ]
pesticides_raw_ohio_insectic <- data.frame(filter(pesticides_raw_ohio, COMPOUND %in% c("PROPARGITE", "PHORATE", "METHOMYL", "METHYL PARATHION", "ETHOPROPHOS", "FONOFOS", "CARBOFURAN")))
pesticides_raw_ohio_insectic$county_id <- str_pad(pesticides_raw_ohio_insectic$COUNTY_FIPS_CODE, 3, "left", pad = "0")
pesticides_raw_ohio_insectic$geoid<-paste(pesticides_raw_ohio_insectic$STATE_FIPS_CODE,pesticides_raw_ohio_insectic$county_id, sep = "")
head(pesticides_raw_ohio_insectic)

names(pesticides_raw_ohio_insectic)[names(pesticides_raw_ohio_insectic)=="YEAR"] <- "Year" #names(d)[names(d)=="beta"] <- "two"

pesticides_raw_ohio_insectic_total <- pesticides_raw_ohio_insectic %>%
  group_by(geoid, Year) %>%
  summarise(
    sum_insect = sum(KG))

pesticides_raw_ohio_insectic_1997 <- subset(pesticides_raw_ohio_insectic, pesticides_raw_ohio_insectic$YEAR=="1997")
head(pesticides_raw_ohio_insectic_1997)
length(unique(pesticides_raw_ohio_insectic_1997$geoid))

names(pesticides_raw_ohio_insectic_1997)
pesticides_raw_ohio_insectic_1997_total <- pesticides_raw_ohio_insectic_1997 %>%
  group_by(geoid) %>%
  summarise(
    sum_insect = sum(KG))
dim(pesticides_raw_ohio_insectic_1997_total)
head(pesticides_raw_ohio_insectic_1997_total)
## Herbicides 
pesticides_raw_ohio_herbs <- data.frame(filter(pesticides_raw_ohio, COMPOUND %in% c("GLYPHOSATE", "ATRAZINE", "ACETOCHLOR", "TRIFLURALIN", "PROPANIL", "ALACHLOR", "EPTC")))
pesticides_raw_ohio_herbs$county_id <- str_pad(pesticides_raw_ohio_herbs$COUNTY_FIPS_CODE, 3, "left", pad = "0")
pesticides_raw_ohio_herbs$geoid<-paste(pesticides_raw_ohio_herbs$STATE_FIPS_CODE,pesticides_raw_ohio_herbs$county_id, sep = "")
head(pesticides_raw_ohio_herbs)

pesticides_raw_ohio_herbs_1997 <- subset(pesticides_raw_ohio_herbs, pesticides_raw_ohio_herbs$YEAR=="1997")
head(pesticides_raw_ohio_herbs)
length(unique(pesticides_raw_ohio_herbs$geoid))

## Landuse
setwd("/Users/Liv/Documents/NCEAS_GIT/NCEAS-RENCI_2014/Landcover/")
landcover_2011 <- read.csv(file =  "OHcounty_lc2011.csv")
head(landcover_2011)
require(reshape2)
landcover_2011_wide <- dcast(landcover_2011, GEOID + NAME + ALAND + INTPTLAT + INTPTLON ~ reclass, value.var = "Class.prop")
  data.wide <- dcast(origdata.long, subject + sex ~ condition, value.var="measurement")
head(landcover_2011_wide)
length(unique(landcover_2011_wide$GEOID)) # 88 
landcover_2011_wide$geoid <- landcover_2011_wide$GEOID

landcover_2001 <- read.csv(file =  "OHcounty_lc2001.csv")
head(landcover_2001)
require(reshape2)
landcover_2001_wide <- dcast(landcover_2001, GEOID + NAME + ALAND + INTPTLAT + INTPTLON ~ reclass, value.var = "Class.prop")
#data.wide <- dcast(origdata.long, subject + sex ~ condition, value.var="measurement")
head(landcover_2001_wide)
length(unique(landcover_2001_wide$GEOID)) # 88 
landcover_2001_wide$geoid <- landcover_2001_wide$GEOID

landcover_2006 <- read.csv(file =  "OHcounty_lc2006.csv")
head(landcover_2006)
landcover_2006_wide <- dcast(landcover_2006, GEOID + NAME + ALAND + INTPTLAT + INTPTLON ~ reclass, value.var = "Class.prop")
head(landcover_2006_wide)
length(unique(landcover_2006_wide$GEOID)) # 88 
landcover_2006_wide$geoid <- landcover_2006_wide$GEOID
View(landcover_2006_wide)
dim(butterfly.diversity)
head(butterfly.diversity)
dim(landcover_2001_wide)


#merging
butter_land <- merge(landcover_2001_wide, butterfly.diversity)
head(butter_land)
dim(butter_land)
length(intersect(butterfly.diversity$geoid, landcover_2001_wide$geoid))
butter_land_insect <- merge(butter_land, pesticides_raw_ohio_insectic_1997_total)

require(lme4)
require(ggplot2)
names(butter_land_insect)
ggplot(butter_land_insect, aes(x = Shannon)) + theme_bw() + geom_histogram()
#butterflies and 1997 ish data
mod1 <- lm(data = butter_land_insect, Shannon ~ forest + urb)
summary(mod1)

#butterflie and 1997 ish data including insect
mod2 <- lm(data = butter_land_insect, Shannon ~ forest + urb + sum_insect)
summary(mod2)

mod3 <- lm(data = butter_land_insect, Shannon ~ forest + sum_insect)
summary(mod3)

mod4 <- lm(data = butter_land_insect, Shannon ~ urb + sum_insect)
summary(mod4)

mod5 <- lm(data = butter_land_insect, Shannon ~ forest)
summary(mod5)

# pretty much nothing is sig for butterfly data.

##birds 
#birds + insect

bird_insec <- merge(bird.diversity.BBS, pesticides_raw_ohio_insectic_total)
head(bird_insec)
dim(bird_insec) 
require(glmmADMB)
mod6 <- lmer(data = bird_insec, Shannon ~ sum_insect + Year + (1|geoid/Route))
summary(mod6)
coefplot2(mod6)

bird_insec$geoid.factor <- as.factor(bird_insec$geoid)
bird_insec$Route.factor <- as.factor(bird_insec$Route)

mod7 <- glmmadmb(data = bird_insec, Shannon ~ sum_insect + Year + (1|geoid.factor/Route.factor), family = "gaussian")
summary(mod7)

mod8 <- glmmadmb(data = bird_insec, Shannon ~ sum_insect : Year + (1|geoid.factor/Route.factor), family = "gaussian")
summary(mod8)
coefplot2(mod8)

landcover_2001_wide$Year <- "2001"
landcover_2006_wide$Year <- "2005"
landcover_2011_wide$Year <- "2009"
landcover_all_wide <- rbind(landcover_2001_wide, landcover_2006_wide, landcover_2011_wide)

bird_insec_years <- subset(bird_insec, bird_insec$Year %in% c(2001, 2005, 2009), drop = TRUE)  ## could still do this - subset out the years by landcover and then do it

head(bird_insec_years)

bird_insec_land <- merge(bird_insec_years, landcover_all_wide)
head(bird_insec_land)
mod9 <- glmmadmb(data = bird_insec_land, 
                 Shannon ~ agr + urb + (1|geoid.factor/Route.factor),
                 family = "gaussian")
summary(mod9)

mod10 <- glmmadmb(data = bird_insec_land, 
                  Shannon ~ agr + urb + Year (1|geoid.factor/Route.factor),
                  family = "gaussian")
summary(mod10)

mod11 <- glmmadmb(data = bird_insec_land, 
                  Shannon ~ agr + urb + sum_insect + (1|geoid.factor/Route.factor),
                  family = "gaussian")
summary(mod11)

mod12 <- glmmadmb(data = bird_insec_land, 
                  Shannon ~ agr + urb + sum_insect + Year + (1|geoid.factor/Route.factor),
                  family = "gaussian")
summary(mod12)

coefplot2(mod12)

mod13 <- glmmadmb(data = bird_insec_land, 
                  Shannon ~ forest + urb + sum_insect + Year + (1|geoid.factor/Route.factor),
                  family = "gaussian")
summary(mod13)

coefplot2(mod13)


save.image(file = "/Users/Liv/Documents/NCEAS_GIT/NCEAS-RENCI_2014/models.Rdata")

## Moran's i
mod10 <- glmmadmb(data = bird_insec_land, 
                  Shannon ~ agr + urb + Year (1|geoid.factor/Route.factor),
                  family = "gaussian")
summary(mod10)
levels(as.factor(bird_insec_land$Year))

mod10_grid <- expand.grid(
  agr = seq(from = 0, to = 100, by = 0.1),
  urb = seq(from = 0, to = 65, by = 0.1),
  Year = c(2001, 2005, 2009))

mod10_grid_agr <- expand.grid(
  agr = seq(from = 0, to = 100, by = 0.1),
  urb = c(10, 40, 70),
  Year = 2009)

mod10_grid_urb <- expand.grid(
  agr = c(10,40,70),
  urb = seq(from = 0, to = 65, by = 0.1),
  Year = c(2001, 2005, 2009))

mod10_predict <- predict(mod10, newdata = mod10_grid, type = "response", se.fit = TRUE)
summary(mod10)
mod10_grid_agr$mod10_predict_agr_response <- predict(mod10, newdata = mod10_grid_agr, type = "response")

ggplot(mod10_grid_agr, aes(x = agr, y = mod10_predict_agr_response, colour = factor(urb)))+
  geom_point()+
  theme_bw()
bird_insec_land$fitted<-fitted(mod10)
bird_insec_land_2009<-subset(bird_insec_land, bird_insec_land$Year == 2009, drop = TRUE)
head(bird_insec_land_2009)
ggplot(data = bird_insec_land_2009, aes(x = agr, y = Shannon))+
  geom_point()+
  geom_smooth(method = gam, formula = y ~ poly(x,2))+
  theme_bw()+
  labs(x = "Proportion of agricultural land", y = "Bird diversity (shannon index)")


mod10_grid_urb$mod10_predict_urb_response <- predict(mod10, newdata = mod10_grid_urb, type = "response")

mod10_predict_agr <- predict(mod10, newdata = mod10_grid_agr, type = "link", se.fit = TRUE)

mod10_predict_urb <- predict(mod10, newdata = mod10_grid_urb, type = "link", se.fit = TRUE)


require(nlme)
mod1.nlme <- nlme(data = bird_insec_land, 
                 Shannon ~ agr + urb + (1|geoid.factor/Route.factor),
                 family = "gaussian")