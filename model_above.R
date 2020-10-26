library(dplyr)

above.site<- read.csv("annual.csv") %>% mutate(ABOVE=CL+CW) %>%
  group_by(site, model,co2) %>% summarise(ABOVE=mean(ABOVE)) %>%
  pivot_wider(names_from = co2, values_from = ABOVE) %>%
  mutate(Cabove=log(ELE/AMB)) %>% ungroup() %>%
  group_by(site) %>% summarise(ABOVE=mean(Cabove),ABOVE.se=sd(Cabove)/sqrt(n()))
write.csv(above.site,"~/Documents/models.above.mean.csv")

above.model<- read.csv("annual.csv") %>% mutate(ABOVE=CL+CW) %>%
  group_by(site, model,co2) %>% summarise(ABOVE=mean(ABOVE)) %>%
  pivot_wider(names_from = co2, values_from = ABOVE) %>%
  mutate(Cabove=log(ELE/AMB))
models <- read.csv("models.csv")
above.model <- left_join(models, dplyr::select(above.model,-AMB,-ELE))
write.csv(above.model,"models.csv")
