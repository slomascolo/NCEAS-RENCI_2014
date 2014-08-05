##CAlifornia


CA.nmds$NMDS1 <- crops.NMDS.4states.CA$points[,1]
CA.nmds$NMDS2 <- crops.NMDS.4states.CA$points[,2]

species.CA.nmds<-data.frame(NMDS1 = crops.NMDS.4states.CA$species[,1], 
                                  NMDS2 =crops.NMDS.4states.CA$species[,2])
species.CA.nmds$species.names<-rownames(species.CA.nmds)

names(CA.nmds)
CA.nmds.plot<-ggplot(CA.nmds, aes(x = NMDS1, y = NMDS2))+
  geom_text(aes(label = County))+
  geom_text(aes(label = State, x = 2, y = 2), hjust = 1, colour = "orange")+
  geom_text(data = species.CA.nmds, colour = "cornflowerblue", 
            aes(label = species.names, x = NMDS1, y = NMDS2))+
  theme_bw()
CA.nmds.plot

ggsave(plot = CA.nmds.plot, file = "/Users/Liv/Dropbox/NCEAS/quickstats/crop.CA.nmds.plot.pdf", width = 210, height = 148, units = "mm", dpi = 600)
