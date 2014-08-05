##North Carolina

NC.nmds$NMDS1 <- crops.NMDS.4states.NC$points[,1]
NC.nmds$NMDS2 <- crops.NMDS.4states.NC$points[,2]

species.NC.nmds<-data.frame(NMDS1 = crops.NMDS.4states.NC$species[,1], 
                            NMDS2 =crops.NMDS.4states.NC$species[,2])
species.NC.nmds$species.names<-rownames(species.NC.nmds)

names(NC.nmds)
NC.nmds.plot<-ggplot(NC.nmds, aes(x = NMDS1, y = NMDS2))+
  geom_text(aes(label = County))+
  xlim(-2,2.1)+
  geom_text(aes(label = State, x = 2, y = 3), hjust = 1, colour = "orange")+
  geom_text(data = species.NC.nmds, colour = "cornflowerblue", 
            aes(label = species.names, x = NMDS1, y = NMDS2))+
  theme_bw()
NC.nmds.plot

ggsave(plot = NC.nmds.plot, file = "/Users/Liv/Dropbox/NCEAS/quickstats/crop.NC.nmds.plot.pdf", width = 210, height = 148, units = "mm", dpi = 600)

require(grid)
require(gridExtra)
arrangeGrob
arrangeGrob
crops.nmds.4statebystate.plot<-multiplot(NC.nmds.plot, CA.nmds.plot, michigan.nmds.plot, ohio.nmds.plot, cols = 2)

