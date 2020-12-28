#library(devtools) 
#install_github("wviechtb/metafor") 

library(metafor)
library(dplyr) 
library(ggplot2)
library(metagear)
make_pct <- function(x) (exp(x) - 1) * 100

df <- read.csv("soilC - combined.csv",na.strings =c("","NA"),stringsAsFactors=TRUE) %>% 
  filter(Experiment != "") %>% rename(biomass=yi) %>% # Remove empty rows
  mutate(SE.SD = as.factor(SE.SD),
         id=paste0(Experiment,sep="_",N,sep="_",Myc)) %>% 
  droplevels() %>%
  mutate (amb.sd=ifelse(SE.SD == "se" | SE.SD == "SE" | is.na(SE.SD), amb.se * sqrt(amb.n),amb.se), # Calculate SD from SE as SE * sqrt(n)
          elev.sd=ifelse(SE.SD == "se" | SE.SD == "SE" | is.na(SE.SD), elev.se * sqrt(elev.n),elev.se))

# SELECT LAST MEASUREMENT ONLY
df <- df %>%
  group_by(id) %>%
  filter(row_number()==n())

df <- df %>% filter(biomass != "NA", # Remove experiments with missing biomass data
                    nyears >= 0.5) # Remove experiments of less than 6 months duration

# Weights based on the number of years and n of the experiments
df$weightsTime <- with(df, ((amb.n * elev.n)/(amb.n + elev.n)) + ((nyears^2)/(2*nyears)))

### Impute missing SD ###
# https://cran.r-project.org/web/packages/metagear/metagear.pdf
#df$imputedSD <- ifelse(is.na(df$amb.sd), "yes", "no")
#set.seed(1)
#df <- impute_SD(test, columnSDnames= c("amb.sd", "elev.sd"), columnXnames=c("amb", "elev"), 
                 #method = "HotDeck_NN", 
 #                range = 10, M = 1)

### effect sizes ###
# 1. Log Ratio of Means (ROM)
rom <- escalc(measure="ROM",n1i=elev.n,n2i=amb.n,m1i=elev,m2i=amb,
             sd1i=elev.sd,sd2i=amb.sd, data=df)
# 2. Raw Mean Difference (MD)
md <- escalc(measure="MD",n1i=elev.n,n2i=amb.n,m1i=elev,m2i=amb, var.names=c("abs","abs.var"),
             sd1i=elev.sd,sd2i=amb.sd, data=df)

### Nearest Neighbour Imputation ###
library(VIM)
vi_rom <- kNN(rom,variable="vi",dist_var=c("elev.n","nyears","yi"), weights = "auto")
vi_md <- kNN(md,variable="abs.var",dist_var=c("elev.n","nyears","abs"), weights = "auto")

dat <- left_join(vi_rom,vi_md) %>% 
  mutate(Vegetation.type=recode(Ecosystem.type, Agricultural="Herbaceous", Grassland="Herbaceous", 
                                Shrubland="Woody", 'Tree Stand' = "Woody", Wetland="Herbaceous"),
         obs=row_number())
  

write.csv(dat,"soilC_meta.csv")
