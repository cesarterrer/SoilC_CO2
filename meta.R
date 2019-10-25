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
