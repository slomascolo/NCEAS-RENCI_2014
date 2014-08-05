##OHIO

ohio.nmds$NMDS1 <- crops.NMDS.4states.OH$points[,1]
ohio.nmds$NMDS2 <- crops.NMDS.4states.OH$points[,2]

species.ohio.nmds<-data.frame(NMDS1 = crops.NMDS.4states.OH$species[,1], 
                         NMDS2 =crops.NMDS.4states.OH$species[,2])
species.ohio.nmds$species.names<-rownames(species.ohio.nmds)

names(ohio.nmds)
ohio.nmds.plot<-ggplot(ohio.nmds, aes(x = NMDS1, y = NMDS2))+
  geom_text(aes(label = County))+
  geom_text(aes(label = State, x = - 1.9, y = 2), hjust = 0, colour = "orange")+
  geom_text(data = species.ohio.nmds, colour = "cornflowerblue", 
            aes(label = species.names, x = NMDS1, y = NMDS2))+
  theme_bw()
ohio.nmds.plot

ggsave(plot = ohio.nmds.plot, file = "/Users/Liv/Dropbox/NCEAS/quickstats/crop.ohio.nmds.plot.pdf", width = 210, height = 148, units = "mm", dpi = 600)
