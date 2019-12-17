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

data <- dat %>% rename ("Ecosystem.type"=Ecosystem.type , "Duration"=nyears,  "Experiment.type"=Experiment_type,
                        "Effect.biomass"=biomass, "Nitrogen.fertilization"=N, "Symbiotic.type"=Myc, "Soil.depth"=Depth..cm.) %>%
  mutate( Cstock = amb) %>% # Standing C stocks
  filter(Symbiotic.type != "NM") # Remove NM species (only 1)

moderators <- c("Ecosystem.type", "Duration",  "Symbiotic.type", "Effect.biomass", 
                "Experiment.type","Disturbance","Nitrogen.fertilization","Cstock","Soil.depth")

########################### MODEL SELECTION #####################

####---------------------------------------------- META-FOREST ----------------------------------------------####
install_github("cjvanlissa/metaforest")
library(metaforest)
library(caret)

###-------------- OPTIMIZATION --------------###
# Set up 10-fold grouped (=clustered) CV
cv_folds <- trainControl(method = "cv", 10)
# Set up a tuning grid for the three tuning parameters of MetaForest
tuning_grid <- expand.grid(whichweights = c("random", "fixed", "unif"),
                           mtry = 2:6,
                           min.node.size = 2:6)
# Select only moderators and vi
X <- data[, c("vi", moderators)]
# Train the model
mf_cv <- train(y = data$yi,
               x = X,
               method = ModelInfo_mf(),
               trControl = cv_folds,
               tuneGrid = tuning_grid,
               num.trees = 20000)
saveRDS(mf_cv, "mf_cv.RData")
# Check result
mf_cv
# Cross-validated R2 of the final model:
mf_cv$results[which.min(mf_cv$results$RMSE), ]$Rsquared
# Extract final model
final <- mf_cv$finalModel
# Plot convergence
plot(final)
# OOB  R2 of the final model:
final$forest$r.squared
# Plot variable importance
cairo_pdf("graphs/VI_metaforest.pdf", width = 8.27)
VarImpPlot(final)
dev.off()

cairo_pdf("graphs/PartialDependence.pdf", width = 8.27)
PartialDependence(final, vars = names(final$forest$variable.importance)[order(final$forest$variable.importance, decreasing = TRUE)],
                  rawdata=TRUE, plot_int = TRUE)
dev.off()

#####-------------- SIMPLE --------------#####
#### Relative effect ####
X.rel <- data[, c("yi","vi", moderators)]
forest.rel <- MetaForest(yi ~ .,
                         data = X.rel,
                         whichweights = "unif", mtry = 4, min.node.size = 2,
                         num.trees = 30000)

forest.rel$forest$r.squared
cairo_pdf("graphs/VI_RELATIVE.pdf", width = 8.27)
VarImpPlot(forest.rel)
dev.off()
cairo_pdf("graphs/PartialDependence_RELATIVE.pdf", width = 8.27)
PartialDependence(forest.rel, vars = names(forest.rel$forest$variable.importance)[order(forest.rel$forest$variable.importance, decreasing = TRUE)],
                  rawdata=TRUE, plot_int = TRUE)
dev.off()


#### Absolute Effect ####
X.abs <- data[, c("abs","abs.var", moderators)] %>% rename(vi=abs.var)

forest.abs <- MetaForest(abs ~ ., 
                         data = X.abs,
                         whichweights = "unif", mtry = 4, min.node.size = 2,
                         num.trees = 30000)

forest.abs$forest$r.squared
cairo_pdf("graphs/VI_ABSOLUTE.pdf", width = 8.27)
VarImpPlot(forest.abs)
dev.off()
cairo_pdf("graphs/PartialDependence_ABSOLUTE.pdf", width = 8.27)
PartialDependence(forest.abs, vars = names(forest.abs$forest$variable.importance)[order(forest.abs$forest$variable.importance, decreasing = TRUE)],
                  rawdata=TRUE, plot_int = TRUE)
dev.off()

################### ------------------------------------------------------------------------------------------#####################
### GLMULTI ###
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_231.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(rJava)
library(glmulti)
dat$obs <- 1:nrow(dat)
rma.mv.glmulti <- function(formula, data, ...)
  rma(formula, vi, data=data, method="ML", random=~1|Site/obs,...)
# No interactions
res <- glmulti(yi ~ Myc + N + biomass + nyears + Experiment_type + Disturbance + Ecosystem.type, data=dat,
               #method="d",
               level=1, fitfunction=rma.glmulti, crit="aicc", confsetsize=128)
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
plot(res, type="s") # Disturbance and Myc

imp <- data.frame(predictor=names(extractRVI(res)) , importance=extractRVI(res))  %>% mutate(predictor= recode(predictor, Ecosystem.type = "Ecosystem type", nyears = "Duration experiment", Experiment_type = "Experiment type",
                                                                                                               biomass = "Effect on biomass", N="Nitrogen fertilization", Myc="Symbiotic type"))


## PLOT ##
ms1 <- ggplot(imp, aes(x=reorder(predictor,importance), y=importance)) +
  #geom_bar(stat='identity',alpha=1,width=.75,aes(fill=X)) + 
  geom_bar(stat='identity',alpha=1,width=.75,fill="#bdbdbd",color="black") + 
  xlab("") + ylab ("Importance of predictors") +
  coord_flip() + xlab(NULL)+ 
  scale_y_continuous(breaks=c(0,.2,.4,.6,.8,1), limits = c(0,1)) +
  geom_hline(yintercept=0.7,col="black",linetype="dashed") +
  theme_cowplot(font_size = 12) +
  theme(axis.ticks.y=element_blank()) 

ggsave("graphs/importance.pdf",ms1,height=4, width=5,useDingbats=F)
ggsave("graphs/importance.png",ms1,height=4, width=5, dpi=900, type = "cairo-png")

### COMMON SOILS ###
limited <- filter(dat,  N=="Nlow")
intact_limited <- filter(dat, Disturbance == "intact", N=="Nlow")

my_rma.glmulti <- function(formula, data, ...) {
  rma(formula, vi, data=data, method="ML", control=list(stepadj=0.5), ...)
}
res_intact_limited <- glmulti(yi ~ Myc + biomass + nyears + Experiment_type + Ecosystem.type, 
                              data=intact_limited,
                              level=1, fitfunction=my_rma.glmulti, crit="aicc", confsetsize=128)
plot(res_intact_limited, type="s")
eval(metafor:::.glmulti)
coef(res_intact_limited)
mmi <- as.data.frame(coef(res_intact_limited))
mmi <- data.frame(Estimate=mmi$Est, SE=sqrt(mmi$Uncond), Importance=mmi$Importance, row.names=row.names(mmi))
mmi$z <- mmi$Estimate / mmi$SE
mmi$p <- 2*pnorm(abs(mmi$z), lower.tail=FALSE)
names(mmi) <- c("Estimate", "Std. Error", "Importance", "z value", "Pr(>|z|)")
mmi$ci.lb <- mmi[[1]] - qnorm(.975) * mmi[[2]]
mmi$ci.ub <- mmi[[1]] + qnorm(.975) * mmi[[2]]
mmi <- mmi[order(mmi$Importance, decreasing=TRUE), c(1,2,4:7,3)]
modsel <- round(mmi, 4)
#write.csv(modsel,"model_selection.csv")

names(extractRVI(res_intact_limited)) 
predictor2 <- c("Ecosystem type", "Duration experiment",  "Symbiotic type", "Effect on biomass", "Experiment type")
imp2 <- data.frame(predictor2, importance=extractRVI(res_intact_limited))

ms2 <- ggplot(imp2, aes(x=reorder(predictor2,importance), y=importance)) +
  #geom_bar(stat='identity',alpha=1,width=.75,aes(fill=X)) + 
  geom_bar(stat='identity',alpha=1,width=.75,fill="#bdbdbd",color="black") + 
  xlab("") + ylab ("Importance of predictors") +
  coord_flip() + xlab(NULL)+ 
  scale_y_continuous(breaks=c(0,.2,.4,.6,.8,1), limits = c(0,1)) +
  geom_hline(yintercept=0.7,col="black",linetype="dashed") +
  theme_cowplot(font_size = 12) +
  theme(axis.ticks.y=element_blank()) 

ggsave("graphs/importance_common.pdf",ms2,height=4, width=5,useDingbats=F)
ggsave("graphs/importance_common.png",ms2,height=4, width=5, dpi=900, type = "cairo-png")
library(grid)
subplot <- plot_grid(NULL, ms2, labels = c("b",""),rel_heights = c(1-(5/7), 1), 
                     hjust = -2,nrow = 2, ncol=1)
prow <- plot_grid(ms1, subplot + annotation_custom(xmin = -Inf, xmax = Inf, ymin=0.9, ymax =0.85, 
                                                   textGrob("Intact, non-fertilized \n soils", gp=gpar(fontface="bold"))),
                  labels = c("a",""),axis="b",
                  hjust = -2,nrow = 1, ncol=2)
save_plot("graphs/ModelSelection.pdf", prow, ncol=2, nrow=1, base_width=4, device = cairo_pdf, fallback_resolution = 1200)
save_plot("graphs/ModelSelection.png", prow, ncol=2, nrow=1, dpi= 800, base_width=3.5,type = "cairo-png")

### MuMIn ###
library(MuMIn)
eval(metafor:::.MuMIn)
full <- rma(yi, vi, data=dat, mods = ~ Myc + N + nyears + biomass + Ecosystem.type + Experiment_type + Disturbance +
              N:Myc + Experiment_type:Myc + Experiment_type:N + biomass:N,
            method="ML", knha=TRUE)
res2 <- dredge(full, trace=2)
subset(res2, delta <= 2, recalc.weights=FALSE)
summary(model.avg(res2, revised.var=FALSE))
MuMIn::importance(res2)
plot(res2)

