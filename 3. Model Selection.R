source("1. Effect Size.R")
install_github("cjvanlissa/metaforest")
library(metaforest)
library(caret)
library(ggplot2)
library(cowplot)
library(ggrepel)
library(rcartocolor)
options(bitmapType="cairo")
library(parallel)
library(doParallel)

# display_carto_all()
#display_carto_pal(12, "Bold")
mycols <- carto_pal(12, "Bold")[c(4,2,1,7)]
mycols_vegsoil <- carto_pal(12, "Bold")[c(2,4)]
make_pct <- function(x) (exp(x) - 1) * 100


data <- dat %>% rename ("Ecosystem.type"=Ecosystem.type , "Duration"=nyears,  "Experiment.type"=Experiment_type,
                        "Effect.biomass"=biomass, "Nitrogen.fertilization"=N, "Symbiotic.type"=Myc, "Soil.depth"=Depth..cm.) %>%
  mutate( Cstock = amb, CO2=log(elevCO2/ambCO2)) %>% # Standing C stocks
  filter(Symbiotic.type != "NM") # Remove NM species (only 1)
data.abs <- data %>% dplyr::select(-yi, -vi) %>% rename(yi=abs, vi=abs.var)
moderators <- c("Ecosystem.type","Vegetation.type", "Duration",  "Symbiotic.type", "Effect.biomass", "MAT", "MAP", "CNr", "P", "pH",
                "Experiment.type","Disturbance","Nitrogen.fertilization","Cstock","Soil.depth", "LAImean","LAImax", "fPARmean", "fPARmax")
newnames <- c("Ecosystem type", "Vegetation type", "Duration",  "Symbiotic type", "CO2 effect on biomass", "MAT", "MAP", "CNr", "P", "pH",
              "Experiment type","Disturbance","Nitrogen fertilization","Soil C stock","Soil depth", "LAImean","LAImax", "fPARmean", "fPARmax")
names(newnames) <- moderators
########################### MODEL SELECTION #####################

####---------------------------------------------- META-FOREST ----------------------------------------------####

###-------------- OPTIMIZATION --------------###
set.seed(36326)
# Check how many iterations metaforest needs to converge
check_conv.abs <- MetaForest(as.formula(paste0("yi~", paste(moderators, collapse = "+"))),
                         data = data.abs,
                         whichweights = "random",
                         num.trees = 10000)
# Plot convergence trajectory
plot(check_conv.abs)
# Perform recursive preselection
set.seed(45)
pre.abs <- preselect(check_conv.abs, replications = 100, algorithm = "recursive")
pre.abs.p <- plot(pre.abs, label_elements = newnames)
ggsave("graphs/preselected_abs.pdf", p, width =  6.875, units = "in")
ggsave("graphs/preselected_abs.png", p, width =  6.875, units = "in")

# Tune the metaforest analysis

# Set up 10-fold CV
set.seed(728)
cv_folds <- trainControl(method = "cv", 10,allowParallel = TRUE)
#repeatedcv <- trainControl(method="repeatedcv", number=10, repeats=3,allowParallel = TRUE)
# Set up a tuning grid for the three tuning parameters of MetaForest
tuning_grid <- expand.grid(whichweights = c("random", "fixed", "unif"),
                           mtry = 2:6,
                           min.node.size = 2:6)

################ ABSOLUTE ###############
# Select only moderators and vi
Y <- dplyr::select(data.abs, "vi", preselect_vars(pre.abs, cutoff = .95), MAP,MAT)
#selected_mods <- c(preselect_vars(preselected, cutoff = .95),"MAT","MAP","Duration")
#saveRDS(selected_mods, file="preselect_mods.RData")
# Train the model
library(parallel)
library(doParallel)
registerDoParallel(31) # Run in parallel -> 31 cores
set.seed(36326)
mf_cv_abs <- train(y = data.abs$yi,
                   x = Y,
                   method = ModelInfo_mf(),
                   trControl = cv_folds,
                   tuneGrid = tuning_grid,
                   keep.inbag = TRUE,
                   num.threads = 31,
                   verbose=TRUE,
                   num.trees = 10000)
saveRDS(mf_cv_abs, "mf_cv_abs.RData")
# Check result
mf_cv_abs # The final values used for the model were whichweights = unif, mtry = 2 and min.node.size = 4
mf_cv_abs$results[which.min(mf_cv_abs$results$RMSE), ]
# Cross-validated R2 of the final model:
mf_cv_abs$results[which.min(mf_cv_abs$results$RMSE), ]$Rsquared # 0.5292879
# Extract final model
forest.abs <- mf_cv_abs$finalModel
# Plot convergence
plot(forest.abs)
# OOB  R2 of the final model:
forest.abs$forest$r.squared
# Plot variable importance
importance.export <- varImp(mf_cv_abs)$importance
write.csv(importance.export,"VI_abs.csv")
imp.abs <- VarImpPlot(forest.abs,label_elements = newnames)
imp.abs
ggsave("graphs/VI_Abs_metaforest.pdf", imp.abs, width =  6.875, units = "in")
ggsave("graphs/VI_Abs_metaforest.png", imp.abs, width =  6.875, units = "in")

# Plot partial dependence
#mf_cv_abs <- readRDS("mf_cv_abs.RData")
#forest.abs <- mf_cv_abs$finalModel
arr <- as.list(newnames)
names(arr) <- moderators
mylabel <- function(val) { return(lapply(val, function(x) arr[x])) }

pi <- PartialDependence(forest.abs, vars = names(forest.abs$forest$variable.importance)[order(forest.abs$forest$variable.importance, decreasing = TRUE)], 
                        rawdata = T, pi = NULL, output = "list",  plot_int = TRUE, bw=TRUE)
p <- lapply(pi, function(x){
  x + facet_wrap(~Variable,labeller=mylabel,ncol = 3) + ylim(c(-300,600)) +
    ylab(expression(paste("Change in soil C stock  (g ", m^-2,")", " in response to ", CO[2] ,sep="")))
})
p[[3]] <- p[[3]]+scale_x_discrete(labels = c("AM", "AMER", "ECM","Nfix"))
p[[4]] <- p[[4]]+scale_x_discrete(labels = c("N-fert.", "non-fert."))
p[[10]] <- p[[10]]+scale_x_discrete(labels = c("Ag", "Gr", "Shr", "For"))
png("graphs/PartialDependence_Absolute.png",width =  8, height = 8, units="in", res=1200)
p.p <- metaforest:::merge_plots(p)
dev.off()

################ RELATIVE ###############
set.seed(36326)
# Check how many iterations metaforest needs to converge
check_conv <- MetaForest(as.formula(paste0("yi~", paste(moderators, collapse = "+"))),
                         data = data,
                         whichweights = "random",
                         num.trees = 10000)
# Plot convergence trajectory
plot(check_conv)
set.seed(45)
pre.rel <- preselect(check_conv, replications = 100, algorithm = "recursive")
pre.rel.p <- plot(pre.rel, label_elements = newnames)
ggsave("graphs/preselected_rel.pdf", pre.rel.p, width =  6.875, units = "in")
ggsave("graphs/preselected_rel.png", pre.rel.p, width =  6.875, units = "in")

# Select only moderators and vi
X <- dplyr::select(data, "vi", preselect_vars(pre.rel, cutoff = .95), MAP,MAT)

# Train the model
library(parallel)
library(doParallel)
registerDoParallel(31) # Run in parallel -> 31 cores
set.seed(36326)
mf_cv <- train(y = data$yi,
                   x = X,
                   method = ModelInfo_mf(),
               trControl = cv_folds,
               tuneGrid = tuning_grid,
               keep.inbag = TRUE,
               num.threads = 31,
               verbose=TRUE,
               num.trees = 10000)
saveRDS(mf_cv, "mf_cv_rel.RData")
importance.export.rel <- varImp(mf_cv)$importance
write.csv(importance.export.rel,"VI_rel.csv")
final <- mf_cv$final
imp.rel <- VarImpPlot(final,label_elements = newnames)
imp.rel
imp <- VarImpPlot(final,label_elements = newnames)
ggsave("graphs/VI_Rel_metaforest.pdf", imp, width =  6.875, units = "in")
ggsave("graphs/VI_Rel_metaforest.png", imp, width =  6.875, units = "in")

# Plot partial dependence
p.rel <- PartialDependence(final, vars = names(final$forest$variable.importance)[order(final$forest$variable.importance, decreasing = TRUE)], 
                           rawdata = T, pi = NULL, output = "list",  plot_int = TRUE, bw=TRUE)
p.rel <- lapply(p.rel, function(x){
  x + facet_wrap(~Variable,labeller=mylabel)
})
p.rel[[4]] <- p.rel[[4]]+scale_x_discrete(labels = c("AM", "AMER", "ECM","Nfix"))
p.rel[[3]] <- p.rel[[3]]+scale_x_discrete(labels = c("N-fer", "non-fert"))
p.rel[[14]] <- p.rel[[14]]+scale_x_discrete(labels = c("Ag", "Gr", "Shr", "For"))
p.rel <- metaforest:::merge_plots(p.rel)
save_plot("graphs/PartialDependence_Relative.pdf", p.rel, base_width =  7, base_height = 6)
save_plot("graphs/PartialDependence_Relative.png", p.rel, base_width =  7, base_height = 6)

# Reduced version
newmods <- c(preselect_vars(pre.abs, cutoff = .95), "MAT", "MAP")
reduced <- MetaForest(as.formula(paste0("yi~", paste(newmods, collapse = "+"))),
                         data = data, keep.inbag = TRUE,
                         whichweights = "unif", mtry = 6,min.node.size = 6,
                       num.trees = 10000)
saveRDS(reduced, "reduced_rel.RData")

############## MULTIPLOT #################
vi.rel <- read.csv("VI_rel.csv") %>% mutate(Predictor=recode(X,"Ecosystem.type"="Ecosystem type", "Symbiotic.type"="Symbiotic type", "Effect.biomass"="CO2 effect on biomass", "Experiment.type"="Experiment type",       
                                                             "Nitrogen.fertilization"="Nitrogen fertilization", "Cstock"="Soil C stock","Soil.depth"="Soil depth"))
RVIrel <- ggplot(vi.rel, aes(x=reorder(Predictor,Overall), y=Overall)) +
  geom_bar(stat='identity',width=.75,color="black", fill="#56B4E9") + 
  xlab("") + ylab ("Relative Importance of predictors") +
  coord_flip() + xlab(NULL) + 
  theme_classic() + 
  theme(legend.title = element_blank(),legend.position = c(0.73, 0.16),
        legend.box.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"))

vi.abs <- read.csv("VI_abs.csv") %>% mutate(Predictor=recode(X,"Ecosystem.type"="Ecosystem type", "Symbiotic.type"="Symbiotic type", "Effect.biomass"="CO2 effect on biomass", "Experiment.type"="Experiment type",       
                                            "Nitrogen.fertilization"="Nitrogen fertilization", "Cstock"="Soil C stock","Soil.depth"="Soil depth"))
RVIabs <- ggplot(vi.abs, aes(x=reorder(Predictor,Overall), y=Overall)) +
  geom_bar(stat='identity',width=.75,color="black", fill="#56B4E9") + 
  xlab("") + ylab ("Relative Importance of predictors") +
  coord_flip() + xlab(NULL) + 
  theme_classic() + 
  theme(legend.title = element_blank(),legend.position = c(0.73, 0.16),
        legend.box.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"))


######################### Non-fertilized ######################
nonfert.model <- MetaForest(yi ~ Symbiotic.type + Effect.biomass +  Experiment.type + Cstock + Ecosystem.type +
                              MAT + MAP + CNr + P + Soil.depth + LAImax + Vegetation.type, 
                            num.trees = 10000,data=filtered)
pre.nonfert <- preselect(nonfert.model, replications = 100, algorithm = "recursive")
nonfert.data <- dplyr::select(filtered, "vi", preselect_vars(pre.nonfert, cutoff = .95))
registerDoParallel(31) # Run in parallel -> 31 cores
set.seed(36326)
mf_cv_nonfert <- train(y = filtered$yi,
                       x = nonfert.data,
                       method = ModelInfo_mf(),
                       trControl = cv_folds,
                       tuneGrid = tuning_grid,
                       keep.inbag = TRUE,
                       num.threads = 31,
                       verbose=TRUE,
                       num.trees = 10000)
saveRDS(mf_cv_nonfert, "mf_cv_nonfert.RData")
mf_cv_nonfert # The final values used for the model were whichweights = random, mtry = 6 and min.node.size = 6
mf_cv_nonfert$results[which.min(mf_cv_nonfert$results$RMSE), ]$Rsquared # 0.7124237
# Extract final model
forest.nonfert <- mf_cv_nonfert$finalModel
# Plot variable importance
importance.export <- varImp(mf_cv_nonfert)$importance

### GLMULTI ###
#dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_231.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
#library(rJava)
extractRVI <- function(x) {
  ww = exp(-(x@crits - x@crits[1])/2)
  ww = ww/sum(ww)
  clartou = function(x) {
    pieces <- sort(strsplit(x, ":")[[1]])
    if (length(pieces) > 1)
      paste(pieces[1], ":", pieces[2], sep = "")
    else x
  }
  tet = lapply(x@formulas, function(x) sapply(attr(delete.response(terms(x)), "term.labels"), clartou))
  allt <- unique(unlist(tet))
  imp <- sapply(allt, function(x) sum(ww[sapply(tet, function(t) x %in% t)]))
  return(sort(imp))
}
library(glmulti)
data$obs <- 1:nrow(data)
filtered <- filter(data, Nitrogen.fertilization=="Nlow", Experiment.type != "Chamber", Disturbance=="intact")
intact <- filter(data, Experiment.type != "Chamber", Disturbance=="intact")
rma.glmulti <- function(formula, data, ...) {
  rma(formula, vi, data=data, method="ML", control=list(stepadj=0.5, optimizer="optimParallel",ncpus=31),...)
}
##################### INTACT ############################
# With interactions
res <- glmulti(yi ~ Nitrogen.fertilization + Symbiotic.type + Effect.biomass + 
                 Experiment.type + Cstock, 
                  data=intact,
               level=2, fitfunction=rma.glmulti, crit="aicc", confsetsize=128
               )
plot(res, type="s")
imp <- data.frame(predictor=names(extractRVI(res)), importance=extractRVI(res))
write.csv(imp, "AICimportance.csv")
summary(res@objects[[1]]) # Best model
aicc <- read.csv("AICimportance.csv")
cutoff <- 0.7
RVIabsAIC <- ggplot(aicc, aes(x=reorder(predictor,importance), y=importance)) +
  geom_bar(stat='identity',width=.75,color="black", fill="#E69F00") + 
  xlab("") + ylab ("Relative Importance of predictors") +
  coord_flip() + xlab(NULL) + 
  #geom_hline(yintercept=cutoff,col="black",linetype="dashed") +
  theme_classic() + 
  theme(legend.title = element_blank(),legend.position = c(0.73, 0.16),
        legend.box.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"))

#################### Bplant ##################
veg <- read.csv("/Users/terrermoreno1/OneDrive - LLNL/FACEreview/data/AGB_effects2019.csv",na.strings=c("",NA),stringsAsFactors=TRUE) %>% 
  dplyr::filter(!is.na(CNr),!is.na(PamountBray09_new)) %>%
  rename ("Ecosystem.type"=System.Type , "Duration"=nyears,  "Experiment.type"=Fumigation.type,
          "Nitrogen.fertilization"=N, "Symbiotic.type"=Myc,Age=Age2, pH=ph, P=PamountBray09_new,TotN=totN.) %>%
  mutate(Ecosystem.type=recode(Ecosystem.type, Scrubland="Shrubland"), Experiment.type=recode(Experiment.type,G="Chamber"))
mods.veg <- c("Symbiotic.type",  "MAT", "MAP", "CNr", "P", "Experiment.type","Nitrogen.fertilization", "Ecosystem.type")
VEG <- dplyr::select(veg,"vi",mods.veg)
library(parallel)
library(doParallel)
registerDoParallel(31) # Run in parallel -> 31 cores
set.seed(36326)
mf_cv_veg <- train(y = veg$yi,
                   x = VEG,
                   method = ModelInfo_mf(),
                   trControl = cv_folds,
                   tuneGrid = tuning_grid,
                   keep.inbag = TRUE,
                   num.threads = 31,
                   verbose=TRUE,
                   num.trees = 10000)
mf_cv_veg # The final values used for the model were whichweights = "fixed", mtry = 2, min.node.size = 6
mf_cv_veg$results[which.min(mf_cv_veg$results$RMSE), ]$Rsquared
importance.export.veg <- varImp(mf_cv_veg)$importance
write.csv(importance.export.veg,"VI_veg.csv")

# Extract final model
veg.final <- mf_cv_veg$finalModel
VarImpPlot(veg.final)
saveRDS(veg.final, "mf_cv_veg.RData")

vi.veg <- read.csv("VI_veg.csv") %>% mutate(Predictor=recode(X,"Ecosystem.type"="Ecosystem type", "Symbiotic.type"="Symbiotic type", "Experiment.type"="Experiment type",       
                                                             "Nitrogen.fertilization"="Nitrogen fertilization", "CNr"="Soil C:N", "P"="Soil available P"))
RVIveg <- ggplot(vi.veg, aes(x=reorder(Predictor,Overall), y=Overall)) +
  geom_bar(stat='identity',width=.75,color="black", fill="seagreen") + 
  xlab("") + ylab ("Relative Importance of predictors") +
  coord_flip() + xlab(NULL) + 
  theme_classic() + 
  theme(legend.title = element_blank(),legend.position = c(0.73, 0.16),
        legend.box.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"))

RVI.p <- plot_grid(RVIrel + theme(plot.margin = unit(c(10,5,5,5), "points")), RVIabs, 
                   RVIabsAIC + theme(plot.margin = unit(c(10,5,5,5), "points")), RVIveg, 
                   nrow=2, ncol=2, align="v", 
                   hjust = -1,vjust =1, labels = "auto")
save_plot("graphs/RVImulti.png",RVI.p, ncol=2, nrow=2, dpi= 800, base_width =  6, base_height = 5, 
          type = "cairo-png", bg="white")
RVI2.p <- plot_grid(RVIabsAIC + theme(plot.margin = unit(c(10,5,5,5), "points")), RVIveg, nrow=2, ncol=1, align="v", 
                   hjust = -1,vjust =1, labels = "auto")
save_plot("graphs/RVImulti2.png",RVI2.p, ncol=1, nrow=2, dpi= 800, base_width =  6, base_height = 5, 
          type = "cairo-png", bg="white")

