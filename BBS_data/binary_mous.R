BBS_traits_highag_frame <- expand.grid(COMMON_NAME = unique(BBS_traits_highag$COMMON_NAME), year = unique(BBS_traits_highag$year), RTENO = unique(BBS_traits_highag$RTENO))

# species n
BBS_traits_highag_spciesn <- BBS_traits_highag %>% group_by(year, RTENO) %>% summarise(n_species = length(unique(COMMON_NAME)))

speciesn <- ggplot(BBS_traits_highag_spciesn, aes(x = year, y = n_species)) +
  geom_line(aes(group = RTENO)) + geom_smooth(method = "glm", family = "poisson")

## shannon
BBS_traits_highag_species2 <- BBS_traits_highag_species %>% mutate(shannon = diversity(BBS_traits_highag_species[-c(1:2)], "shannon")) %>% select(RTENO, year, shannon)

shannon <- ggplot(BBS_traits_highag_species2, aes(x = year, y = shannon)) + geom_line(aes(group = RTENO)) + ggtitle("Shannon (species evenness)")


all_graphs <- arrangeGrob(diet,  conservation, speciesn,behavior, shannon, all_birds, ncol = 3)
all_graphs

## species evenness
BBS_traits_highag_species <- BBS_traits %>% filter(RTENO %in% high_ag92$RTENO)%>% select(RTENO, year, sum_route_abundance, COMMON_NAME) %>% ungroup() %>% group_by(RTENO, year, COMMON_NAME) %>% summarise(sum_route_abundance = mean(sum_route_abundance)) %>% spread(key = COMMON_NAME, value = sum_route_abundance, fill = 0 )
