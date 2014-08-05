##Michigan

michigan.nmds$NMDS1 <- crops.NMDS.4states.MI$points[,1]
michigan.nmds$NMDS2 <- crops.NMDS.4states.MI$points[,2]

species.michigan.nmds<-data.frame(NMDS1 = crops.NMDS.4states.MI$species[,1], 
                              NMDS2 =crops.NMDS.4states.MI$species[,2])
species.michigan.nmds$species.names<-rownames(species.michigan.nmds)

names(michigan.nmds)
michigan.nmds.plot<-ggplot(michigan.nmds, aes(x = NMDS1, y = NMDS2))+
  geom_text(aes(label = County))+
  geom_text(aes(label = State, x = - 1.9, y = 2), hjust = 0, colour = "orange")+
  geom_text(data = species.michigan.nmds, colour = "cornflowerblue", 
            aes(label = species.names, x = NMDS1, y = NMDS2))+
  theme_bw()
michigan.nmds.plot

ggsave(plot = michigan.nmds.plot, file = "/Users/Liv/Dropbox/NCEAS/quickstats/crop.michigan.nmds.plot.pdf", width = 210, height = 148, units = "mm", dpi = 600)
