library(devtools) 
install_github("wviechtb/metafor") 
library(metafor)
library(dplyr) 
library(ggplot2)
library(metagear)
make_pct <- function(x) (exp(x) - 1) * 100

df <- read.csv("soilC - combined.csv",na.strings = "")
df = df %>% filter(Experiment != "") %>% rename(biomass=yi) # Remove empty rows
df$id <- with(df,paste0(Experiment,sep="_",N,sep="_",Myc)) # Create ID column
df <- droplevels(df)


# Calculate SD from SE as SE * sqrt(n)
df <- df %>%  
  mutate (amb.sd=ifelse(SE.SD == "se" | SE.SD == "SE", amb.se * sqrt(amb.n),amb.se),
          elev.sd=ifelse(SE.SD == "se" | SE.SD == "SE", elev.se * sqrt(elev.n),elev.se))

# SELECT LAST MEASUREMENT ONLY! (we can change this later on if we decide so)
df <- df %>%
  group_by(id) %>%
  filter(row_number()==n())


# Weights based on the number of years and n of the experiments
df$weightsTime <- with(df, ((amb.n * elev.n)/(amb.n + elev.n)) + ((nyears^2)/(2*nyears)))

### Impute missing SD ###
# https://cran.r-project.org/web/packages/metagear/metagear.pdf
df$imputedSD <- ifelse(is.na(df$amb.sd), "yes", "no")
set.seed(1)
df <- impute_SD(df, columnSDnames= c("amb.sd", "elev.sd"), columnXnames=c("amb", "elev"), 
                 #method = "HotDeck_NN", 
                 range = 10, M = 1)

### Meta-analysis ###
# 1. Calculate the data neccesary to run the meta-analysis
df <- escalc(measure="ROM",n1i=elev.n,n2i=amb.n,m1i=elev,m2i=amb,
             sd1i=elev.sd,sd2i=amb.sd, data=df) # Here we simply create our metric, in this case the ratio of means (ROM). R automatically calculates ROM,
# as the ratio of the mean under elevated and mean under ambient CO2. We also tell R what are the columns with the values for sample suze (n1i, n2i) and standard deviation (sd1i, sd2i)
# if you want more information about this step, run ?escalc in the console

write.csv(df,"soilC_meta.csv")

df <- filter(df, !is.na(biomass))
# 2. Run meta-analysis
m1 <- summary (rma(yi, vi, mods=~ biomass*N -1, 
                   weights=weightsTime,
                   data=df))
m1 # Here we run a meta-analysis separating responses across Myc (mycorrhizal type) and N (nitrogen availability)
# The output tells us that the effect of CO2 on soil C in ECM-lowNitrogen plants is negative (-0.0958), and positive in the rest of subsets

# ECM - High N
ECMh <- rma(yi, vi, data=df, subset=N=="Nhigh" & Myc=="ECM")
ECMh.n <- ECMh$k

# ECM - Low N
ECMl <- rma(yi, vi, data=df, subset=N=="Nlow" & Myc=="ECM")
ECMl.n <- ECMl$k

# AM - high N
AMh <- rma(yi, vi, data=df, weights=weightsTime,subset=N=="Nhigh" & Myc=="AM")
AMh.n <- AMh$k

# AM - low N
AMl <- rma(yi, vi, data=df, weights=weightsTime,subset=N=="Nlow" & Myc=="AM")
AMl.n <- AMl$k

fig <- data.frame(es=make_pct(m1$b),ll=make_pct(m1$ci.lb),
                  ul=make_pct(m1$ci.ub), 
                  se=make_pct(m1$se),
                  P=round(m1$pval,4),
                  n=c(AMh.n, AMl.n,ECMh.n, ECMl.n),
                  Myc=c("AM","AM","ECM","ECM"), N=rep(c("High N","Low N"),2))

ggplot(data=fig,aes(x=N,y=es,ymax=ul,ymin=ll,fill=Myc,group=interaction(Myc,N),label=paste("(n=",n,")",sep="")))+ 
  geom_errorbar(width=.0,size=.36,position=position_dodge(width=0.3)) + 
  geom_point(colour="black",pch=21,size=5,position=position_dodge(width=0.3)) + 
  scale_size_manual(values=c(6,8)) +
  xlab("N availability") + ylab(expression(paste(CO[2]," effect on soil C content (%)", sep=""))) + 
  geom_text(data=fig[fig$Myc=="ECM",],aes(vjust=-2.5),size=12/2.8,colour="black") +
  geom_text(data=fig[fig$Myc=="AM",],aes(vjust=3.5),size=12/2.8,colour="black") +
  scale_fill_grey(start = 1, end = 0) +
  scale_colour_grey(start = 0, end = 0.5) +
  theme_bw() +
  xlab(NULL) +
  coord_flip() +
  geom_hline(yintercept = 0, lty=2, size=1) +
  theme(legend.justification=c(.99,.99), legend.position=c(.99,.99),legend.title=element_blank(),
        legend.direction = "vertical",axis.text.y = element_text(size=14))
ggsave("graphs/SoilCmeta.pdf",height=6, width=6)

# ECM - LOW N experiments
pdf("graphs/soilCECMlow.pdf")
ecm <- rma(yi, vi, data=df,weights=weightsTime, slab=as.character(Experiment),
           subset=N=="Nlow" & Myc=="ECM")
forest(ecm,transf=make_pct, mlab ="Soil C ECM - low N")
dev.off()

# AM - LOW N experiments
pdf("graphs/soilCAMlow.pdf")
am <- rma(yi, vi, data=df,weights=weightsTime, slab=as.character(Experiment),
          subset=N=="Nlow" & Myc=="AM")
forest(am,transf=make_pct, mlab ="Soil C AM - low N")
dev.off()

# Under N-limitations, is soil C content in AM > ECM?
summary (rma(yi, vi, mods=~ Myc, data=df,weights=weightsTime, 
             subset=N=="Nlow")) # YESS! pval < 0.05
