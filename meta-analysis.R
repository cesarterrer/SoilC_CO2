source("Effect Size.R")
library(ggplot2)
library(cowplot)
library(ggrepel)
library(rcartocolor)
options(bitmapType="cairo")
# display_carto_all()
display_carto_pal(12, "Bold")
mycols <- carto_pal(12, "Bold")[c(4,2,1,7)]
mycols_vegsoil <- carto_pal(12, "Bold")[c(2,4)]
make_pct <- function(x) (exp(x) - 1) * 100

#dat <- read.csv("soilC_meta.csv")
dat <- filter(dat, biomass != "NA") # Remove experiments with missing biomass data
dat <- filter(dat, nyears >= 0.5) # Remove experiments of less than 6 months duration

# Nitrogen
Nm <- rma(yi, vi, data=dat, mods=~N -1, knha=TRUE)
Nm
Nm.n <- dat %>%  group_by(N) %>% summarise(n = n())
Nm.df <- coef(summary(Nm)) %>% mutate(type="Nitrogen fertilization", 
                                          factor=as.factor(c("Yes", "No")),
                                          size=Nm.n$n)


# Ecosystem.type
Ecom <- rma(yi, vi, data=dat, mods=~Ecosystem.type -1, knha=TRUE, subset= Ecosystem.type !="Wetland")
Ecom
Ecom.n <- dat %>%  group_by(Ecosystem.type) %>% summarise(n = n())
Ecom.df <- coef(summary(Ecom)) %>% mutate(type="Ecosystem type", 
                                          factor=levels(dat$Ecosystem.type)[1:4],
                                          size=Ecom.n$n[1:4])
# Experiment.type
Expm <- rma(yi, vi, data=dat, mods=~Experiment_type -1, knha=TRUE)
Expm
Expm.n <- dat %>%  group_by(Experiment_type) %>% summarise(n = n())
Expm.df <- coef(summary(Expm)) %>% mutate(type="Experiment type", 
                                              factor=levels(dat$Experiment_type),
                                              size=Expm.n$n)
# Biome
Biomem <- rma(yi, vi, data=dat, mods=~Biome -1, knha=TRUE)
Biomem
Biomem.n <- dat %>%  group_by(Biome) %>% summarise(n = n())
Biomem.df <- coef(summary(Biomem)) %>% mutate(type="Biome", 
                                                          factor=levels(dat$Biome),
                                                          size=Biomem.n$n)
# Disturbance
Disturbancem <- rma(yi, vi, data=dat, mods=~Disturbance -1, knha=TRUE)
Disturbancem
Disturbancem.n <- dat %>%  group_by(Disturbance) %>% summarise(n = n())
Disturbancem.df <- coef(summary(Disturbancem)) %>% mutate(type="Disturbance", 
                                          factor=c("Disturbed","Intact"),
                                          size=Disturbancem.n$n)

# Myc
Mycm <- rma(yi, vi, data=dat, mods=~Myc -1, knha=TRUE, subset= Myc != "NM")
Mycm.n <- dat %>%  group_by(Myc) %>% summarise(n = n()) %>% filter(Myc != "NM")
Mycm.df <- coef(summary(Mycm)) %>% mutate(type="Nutrient-acquisition strategy", 
                                          factor=c("AM","EcM","ER","N-fixer"),
                                          size=Mycm.n$n)
#### META PLOT ####
meta.df <- bind_rows(Ecom.df, Nm.df, Expm.df, Disturbancem.df, Mycm.df)

pdf("graphs/figure1.pdf",height=5, width=3, useDingbats=FALSE)
par(mar=c(4,4,1,2))
forest(x=meta.df$estimate,sei=meta.df$se,slab=meta.df$factor, annotate=FALSE, xlim=c(-30, 10),
       ilab=paste0("(",meta.df$size,")"),ilab.xpos=-11,
       psize=1,transf=make_pct, at=c(-10, 0, 10, 20), xlab=expression(paste(CO[2]," effect on soil C (%)", sep="")),
       subset=15:1, rows=c(1:4,7:8,11:13,16:17,20:23),ylim=c(-1, 27),cex=0.75)
op <- par(cex=.75, font=4)
text(-30, c(5,9,14,18,24), pos=4, c("Nutrient strategy","Disturbance","Experiment type", "Nitrogen fertilization", "Ecosystem type"))
par(cex=0.75, font=1)
dev.off()

png("graphs/figure1.png",height=5, width=3, units ="in", res = 300, type = "cairo")
par(mar=c(4,4,1,2))
forest(x=meta.df$estimate,sei=meta.df$se,slab=meta.df$factor, annotate=FALSE, xlim=c(-30, 10),
       ilab=paste0("(",meta.df$size,")"),ilab.xpos=-11,
       psize=1,transf=make_pct, at=c(-10, 0, 10, 20), xlab=expression(paste(CO[2]," effect on soil C (%)", sep="")),
       subset=15:1, rows=c(1:4,7:8,11:13,16:17,20:23),ylim=c(-1, 27),cex=0.75)
text(-30, c(5,9,14,18,24), pos=4, c("Nutrient strategy","Disturbance","Experiment type", "Nitrogen fertilization", "Ecosystem type"),
     font=2, cex=0.75)
dev.off()




