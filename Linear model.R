

lm_out <- lm(formula = yi~ Effect.biomass*Nitrogen.fertilization + I(Effect.biomass^2)*Nitrogen.fertilization,
             data = data.abs)
#summary(lm_out)
cv_folds_lm <- trainControl(method="repeatedcv", number=10, repeats=10)
set.seed(707)
lm_cv <- caret::train(y = data.abs$yi,
               x = model.matrix(~Effect.biomass*Nitrogen.fertilization, data.abs)[,-1],
               method = "lm",
               trControl = cv_folds_lm
)
lm_cv$results[which.min(lm_cv$results$RMSE), ]$Rsquared
