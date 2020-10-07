# SoilC_CO2
## Overview
Here, we collect data on the effects of elevated CO2 on soil carbon stocks (βsoil) in both relative and absolute terms and synthesize them through meta-analysis. We also collect data on climatic, experimental, and vegetation characteristics that could potentially explain variability in βsoil (“predictors”). In Fig. 1, we show a classic descriptive meta-analysis of overall βsoil across different predictor factors. We next combine the strengths of meta-analysis and random-forest –MetaForest– to quantify the relative importance of 19 predictors in explaining βsoil in the dataset. 
In Fig. 2, we describe the regression between βsoil and its most important predictor (βplant), and explore the reasons explaining the sign and nature of this relationship. In Fig. 3, we apply the data-trained MetaForest model in absolute terms to upscale βsoil globally. Finally, we investigate whether the emerging relationship between βsoil and βplant found in experiments in represented in models (Fig. 4).

## 1. Effect Size.R
Calculate effect sizes of the effect of eCO2 on SOC in absolute and relative terms in each experiment. 

## 2. Pre-meta-analysis.R
Classic meta-analysis showing the relative effect of eCO2 on SOC across different predictors (Fig. 1). 

## 3. Model Selection.R
MetaForest-based variable importance and model selection

## 4. Drivers_Plots.R
Regression analysis of the relationship between βsoil and the most important driver of βsoil. Graphical exploration of potentials factors explaining such relationship (Fig. 2), including N-uptake, MAOM, root biomass, fine-root production.

## 5. Upscaling_MF.R
Upscaling βsoil using the metaforest model from 3. and global data of the predictors of βsoil

## 6. Plot_maps.R
Making pretty maps with the results from 5.

## 7. Zonal_Stats.R
Table showing ecosystem-level βsoil derived from the global results.

## 8. Models and observations.R
Relationship between βsoil and βplant in model ensembles (Fig. 4)

## Aboveground_prod.R
Plotting the relationship between βsoil and CO2-driven changes in ANPP

## CMIP.R
Calculate expected βsoil with CMIP models

## Litter.R
Plotting the relationship between the effects of eCO2 on litter production and biomass production

## model_above.R
Calculate aboveground biomass from FACE-MDS

## Uncert_Hist.R
Plot histograms describing how well the training data represent global data on the predictors used to upscale βsoil.




