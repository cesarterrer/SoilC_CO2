source("1. Effect Size.R")
install_github("cjvanlissa/metaforest")
library(metaforest)
library(caret)
library(ggplot2)
library(cowplot)
library(ggrepel)
library(rcartocolor)
options(bitmapType="cairo")
# display_carto_all()
#display_carto_pal(12, "Bold")
mycols <- carto_pal(12, "Bold")[c(4,2,1,7)]
mycols_vegsoil <- carto_pal(12, "Bold")[c(2,4)]
make_pct <- function(x) (exp(x) - 1) * 100

#dat <- read.csv("soilC_meta.csv")
dat <- filter(dat, biomass != "NA") # Remove experiments with missing biomass data
dat <- filter(dat, nyears >= 0.5) # Remove experiments of less than 6 months duration

data <- dat %>% rename ("Ecosystem.type"=Ecosystem.type , "Duration"=nyears,  "Experiment.type"=Experiment_type,
                        "Effect.biomass"=biomass, "Nitrogen.fertilization"=N, "Symbiotic.type"=Myc, "Soil.depth"=Depth..cm.) %>%
  mutate( Cstock = amb, CO2=log(elevCO2/ambCO2)) %>% # Standing C stocks
  filter(Symbiotic.type != "NM") # Remove NM species (only 1)
data.abs <- data %>% dplyr::select(-yi, -vi) %>% rename(yi=abs, vi=abs.var)
moderators <- c("Ecosystem.type", "Duration",  "Symbiotic.type", "Effect.biomass", "MAT", "MAP",
                "Experiment.type","Disturbance","Nitrogen.fertilization","Cstock","Soil.depth", "LAImean","LAImax", "fPARmean", "fPARmax")
newnames <- c("Ecosystem type", "Duration",  "Symbiotic type", "Effect biomass", "MAT", "MAP",
              "Experiment type","Disturbance","Nitrogen fertilization","Soil C stock","Soil depth", "LAImean","LAImax", "fPARmean", "fPARmax")
names(newnames) <- moderators
########################### MODEL SELECTION #####################

####---------------------------------------------- META-FOREST ----------------------------------------------####

###-------------- OPTIMIZATION --------------###
set.seed(36326)
# Check how many iterations metaforest needs to converge
check_conv <- MetaForest(as.formula(paste0("yi~", paste(moderators, collapse = "+"))),
                         data = data.abs,
                         whichweights = "random",
                         num.trees = 10000)
# Plot convergence trajectory
plot(check_conv)

# Perform recursive preselection
set.seed(45)
preselected <- preselect(check_conv, replications = 100, algorithm = "recursive")
saveRDS(preselected, "preselected_rep.RData")
p <- plot(preselected, label_elements = newnames)
ggsave("graphs/preselected.pdf", p, width =  6.875, units = "in")
ggsave("graphs/preselected.png", p, width =  6.875, units = "in")

# Tune the metaforest analysis

# Set up 10-fold CV
set.seed(728)
cv_folds <- trainControl(method = "cv", 10)
repeatedcv <- trainControl(method="repeatedcv", number=10, repeats=3)
# Set up a tuning grid for the three tuning parameters of MetaForest
tuning_grid <- expand.grid(whichweights = c("random", "fixed", "unif"),
                           mtry = 2:6,
                           min.node.size = 2:6)

################ ABSOLUTE ###############
# Select only moderators and vi
Y <- dplyr::select(data.abs, "vi", preselect_vars(preselected, cutoff = .95))
selected_mods <- preselect_vars(preselected, cutoff = .95)
saveRDS(selected_mods, file="preselect_mods.RData")
# Train the model
set.seed(36326)
mf_cv_abs <- train(y = data.abs$yi,
               x = Y,
               method = ModelInfo_mf(),
               trControl = repeatedcv,
               tuneGrid = tuning_grid,
               keep.inbag = TRUE,
               num.trees = 10000)
saveRDS(mf_cv_abs, "mf_cv_abs.RData")
# Check result
mf_cv_abs # The final values used for the model were whichweights = unif, mtry = 2 and min.node.size = 4
# Cross-validated R2 of the final model:
mf_cv_abs$results[which.min(mf_cv_abs$results$RMSE), ]$Rsquared # 0.5292879
# Extract final model
forest.abs <- mf_cv_abs$finalModel
# Plot convergence
plot(forest.abs)
# OOB  R2 of the final model:
forest.abs$forest$r.squared
# Plot variable importance
imp.abs <- VarImpPlot(forest.abs,label_elements = newnames)
ggsave("graphs/VI_Abs_metaforest.pdf", imp.abs, width =  6.875, units = "in")
ggsave("graphs/VI_Abs_metaforest.png", imp.abs, width =  6.875, units = "in")

# Plot partial dependence
arr <- as.list(newnames)
names(arr) <- moderators
mylabel <- function(val) { return(lapply(val, function(x) arr[x])) }

pi <- PartialDependence(forest.abs, vars = names(forest.abs$forest$variable.importance)[order(forest.abs$forest$variable.importance, decreasing = TRUE)], rawdata = T, pi = NULL, output = "list",  plot_int = TRUE)
p <- lapply(pi, function(x){
  x + facet_wrap(~Variable,labeller=mylabel, nrow=2, ncol = 5) + ylim(c(-500,600))
})
p[[7]] <- p[[7]]+scale_x_discrete(labels = c("N-fert.", "non-fert."))
p[[8]] <- p[[8]]+scale_x_discrete(labels = c("Ag", "Gr", "Shr", "For"))
p.p <- metaforest:::merge_plots(p)
save_plot("graphs/PartialDependence_Absolute.pdf", p.p, base_width =  7, base_height = 6)
save_plot("graphs/PartialDependence_Absolute.png", p.p, base_width =  7, base_height = 6)

################ RELATIVE ###############
# Select only moderators and vi
X <- dplyr::select(data, "vi", preselect_vars(preselected, cutoff = .95))
# Train the model
set.seed(36326)
mf_cv <- train(y = data$yi,
                   x = X,
                   method = ModelInfo_mf(),
                   trControl = repeatedcv,
                   tuneGrid = tuning_grid,
                   num.trees = 10000)
saveRDS(mf_cv, "mf_cv_rel.RData")
# Cross-validated R2 of the final model:
mf_cv$results[which.min(mf_cv$results$RMSE), ]$Rsquared
# Extract final model
final <- mf_cv$final
# Plot convergence
plot(final)
# OOB  R2 of the final model:
final$forest$r.squared
# Plot variable importance
imp <- VarImpPlot(final,label_elements = newnames)
ggsave("graphs/VI_Rel_metaforest.pdf", imp, width =  6.875, units = "in")
ggsave("graphs/VI_Rel_metaforest.png", imp, width =  6.875, units = "in")

# Plot partial dependence
arr <- as.list(newnames)
names(arr) <- moderators
mylabel <- function(val) { return(lapply(val, function(x) arr[x])) }

p <- PartialDependence(final, vars = names(final$forest$variable.importance)[order(final$forest$variable.importance, decreasing = TRUE)], 
                       rawdata = T, pi = .95, output = "list", bw = TRUE)
p <- lapply(p, function(x){
  x + facet_wrap(~Variable,labeller=mylabel)
})
p[[3]] <- p[[3]]+scale_x_discrete(labels = c("N-fertilized", "non-fertilized"))
p[[10]] <- p[[10]]+scale_x_discrete(labels = c("Ag", "Gr", "Shr", "For"))
png("graphs/PartialDependence_Relative.png",  width = 6.875 , height = 6.8, units = "in", res = 300)
metaforest:::merge_plots(p)
dev.off()
cairo_pdf("graphs/PartialDependence_Relative.pdf",  width = 6.875 , height = 6.8)
metaforest:::merge_plots(p)
dev.off()

### GLMULTI ###
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_231.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(rJava)
library(glmulti)
data$obs <- 1:nrow(data)
filtered <- filter(data, Nitrogen.fertilization=="Nlow", Experiment.type != "Chamber", Disturbance=="intact")
intact <- filter(data, Experiment.type != "Chamber", Disturbance=="intact")
rma.glmulti <- function(formula, data, ...) {
  rma(formula, vi, data=data, method="ML", control=list(stepadj=0.5), ...)
}
# No interactions
res <- glmulti(yi ~ Nitrogen.fertilization + Symbiotic.type + Effect.biomass + 
                 Experiment.type + Cstock + Soil.depth, 
                  data=intact,
               level=2, fitfunction=rma.glmulti, crit="aicc", confsetsize=128
               )
saveRDS(res, "glmulti_rel_intact.RData")
plot(res, type="s")
summary(res@objects[[1]]) # Best model
summary(rma(yi, vi, mods= ~biomass+Cstock*Symbiotic.type, data=intact, knha=TRUE)) # Significant interaction
