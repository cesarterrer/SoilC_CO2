library(tidyverse)
library(ggrepel)
library(cowplot)
lit <- read.csv("Litter_Database.csv",na.strings ="NA") %>% mutate(litter=log(T_M/C_M)) %>% 
  group_by(id=Experiment) %>% summarise (litter=mean(litter))

soc_all <- read.csv("soilC_prod_meta.csv") %>% 
  right_join(lit)

soc_intact <- read.csv("soilC_prod_meta.csv") %>% 
  right_join(lit) %>%
  filter(N=="Nlow", Experiment_type != "Chamber", Disturbance=="intact")
  

summary(lm(yi~litter,soc_intact))
summary(lm(litter~biomass,soc_intact))
cor.test(soc_intact$litter, soc_intact$biomass)

soc_lit <- ggplot(soc_intact,aes(litter,yi,label=id)) + geom_point() + ylab("SOC") +
  geom_smooth(size=.5, method = "lm", se=FALSE, color="#e41a1c", formula = y ~ x) +
  labs(x=expression(paste(eCO[2]," effect on litter production (%)", sep="")),
       y=expression(paste(eCO[2]," effect on soil carbon stocks (%)", sep=""))) +
  geom_text_repel() +
  theme_cowplot(font_size=8)

lit_prod <- ggplot(soc_intact,aes(make_pct(biomass),make_pct(litter),label=id)) + 
  geom_point(size=3) + 
  geom_smooth(size=1, method = "lm", se=TRUE, alpha=0.1, color="black", formula = y ~ x) +
  labs(x=expression(paste(eCO[2]," effect on aboveground biomass production (%)", sep="")),
       y=expression(paste(eCO[2]," effect on litter production (%)", sep=""))) +
  theme_cowplot(font_size=8)

save_plot("graphs/litter.png", lit_prod, base_height = 2.25, base_width = 2.5, dpi=1200, type = "cairo-png")
