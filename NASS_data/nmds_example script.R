ohio.nmds #data
crops.NMDS.4states.OH #nmds object
crops.NMDS.4states.MI<-metaMDS(michigan.nmds)
crops.NMDS.4states.NC<-metaMDS(NC.nmds)
crops.NMDS.4states.CA<-metaMDS(CA.nmds)

data$NMDS1 <- nmds$points[,1]
data$NMDS2 <- nmds$points[,2]

species.data<-data.frame(NMDS1 = nmds$species.data[,1], 
                    NMDS2 =nmds$species.data[,2])
species.data$species.names<-rownames(species)

names(data)
data.plot<-ggplot(data, aes(x = NMDS1, y = NMDS2))+
  geom_text(aes(label = County))+
  geom_text(data = species.data, colour = "cornflowerblue", 
            aes(label = species.names, x = NMDS1, y = NMDS2))+
  theme_bw()
data.plot

ggsave(plot = data.plot, file = "/Users/Liv/Dropbox/NCEAS/quickstats/crop.data.plot.pdf", width = 210, height = 148, units = "mm", dpi = 600)