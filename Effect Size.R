library(devtools) 
install_github("wviechtb/metafor") 
library(metafor)
library(dplyr) 
library(ggplot2)
library(metagear)
make_pct <- function(x) (exp(x) - 1) * 100

df <- read.csv("soilC - combined.csv",na.strings =c("","NA"))
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

### effect sizes ###
# 1. Log Ratio of Means (ROM)
rom <- escalc(measure="ROM",n1i=elev.n,n2i=amb.n,m1i=elev,m2i=amb,
             sd1i=elev.sd,sd2i=amb.sd, data=df)
# 2. Raw Mean Difference (MD)
md <- escalc(measure="MD",n1i=elev.n,n2i=amb.n,m1i=elev,m2i=amb, var.names=c("abs","abs.var"),
             sd1i=elev.sd,sd2i=amb.sd, data=df)
dat <- left_join(rom,md)
write.csv(dat,"soilC_meta.csv")
