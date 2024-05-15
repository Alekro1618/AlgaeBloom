#multiple linear regression
lm.a1 = lm(a1 ~ ., data = algae_clean[,1:12])
summary(lm.a1)
#doing backward elimination
anova(lm.a1)
#the result yields that season has the biggest percentile of F-value, so we remove it
lm2.a1 = update(lm.a1, . ~ . -season)
summary(lm2.a1)
anova(lm.a1, lm2.a1)
#doing the algorithm again to the end to get final model
final.lm = step(lm.a1)
summary(final.ln)

#regression tree
library(rpart)
library(insight)
library(DMwR2)
rt.a1 = rpart(a1 ~ ., data = algae_clean[,1:12])
printcp(rt.a1)


#comparring models
lm.pred = predict(final.lm, algae_clean)
rt.pred = predict(rt.a1, algae_clean)
MAE.lm = mean(abs(lm.pred - algae_clean[, 'a1']))
MAE.rt = mean(abs(rt.pred - algae_clean[, 'a1']))
par(mfrow = c(1,2))
plot(lm.pred, algae_clean[,'a1'], main='linear model', xlab = 'predictions', ylab = 'true values')
abline(0, 1, lty = 2)
plot(rt.pred, algae_clean[,'a1'], main='Regression tree', xlab = 'predictions', ylab = 'true values')
abline(0, 1, lty = 2)
algae[identify(lm.pred, algae_clean[,'a1']),]


#since none are good enough we do some cross-validation
rt_c = function(form, train, test) {
  model = rpart(form, train)
  pred = predict(model, test)
  mse = mean(abs(pred-t(test[,find_response(model)])))
  c(mse, list(model))
}
lm_c = function(form, train, test) {
  model = lm(form, train)
  model = step(model, steps = 4)
  pred = predict(model, test)
  pred = ifelse(pred < 0, 0, pred)
  mse = mean(abs(pred-t(test[,find_response(model)])))
  c(mse, list(model))
}
cross_val = function(form, df, k, m) {
  best = 0
  best_score = Inf
  n = floor(nrow(df)/k)
  print(sprintf("Dividing data frame with parts of size %s", n))
  for (i in 1:k) {
    if(i < k) {
      place = (1+(i-1)*n):(i*n)
    } else {
      place = (1+(i-1)*n):nrow(df)
    }
    train = df[-place,]
    test = df[place,]
    print(sprintf("Training %s sample", i))
    score = m(form, train, test)
    print(sprintf("The MSE is: %s", score[1]))
    if (score[1] < unlist(best_score[1])) {
      best_score = score
      best = i
    }
    
  }
  print(sprintf("The least error has sample %s with error: %s", best, unlist(best_score[1])))
  c(best_score[1], list(best_score[[2]]))
}
#doing some model evaluation
models = c(a1 ~ season + size + speed + mxPH + mnO2 + Cl + NO3 + NH4 + oPO4 + PO4 + Chla,
           a2 ~ season + size + speed + mxPH + mnO2 + Cl + NO3 + NH4 + oPO4 + PO4 + Chla,
           a3 ~ season + size + speed + mxPH + mnO2 + Cl + NO3 + NH4 + oPO4 + PO4 + Chla,
           a4 ~ season + size + speed + mxPH + mnO2 + Cl + NO3 + NH4 + oPO4 + PO4 + Chla,
           a5 ~ season + size + speed + mxPH + mnO2 + Cl + NO3 + NH4 + oPO4 + PO4 + Chla,
           a6 ~ season + size + speed + mxPH + mnO2 + Cl + NO3 + NH4 + oPO4 + PO4 + Chla,
           a7 ~ season + size + speed + mxPH + mnO2 + Cl + NO3 + NH4 + oPO4 + PO4 + Chla)
final_model = list()
for(m in models) {
  #choosing best from them
  model.rt = cross_val(m, algae_clean, 2, rt_c)
  model.lm = cross_val(m, algae_clean, 2, lm_c)
  if(model.rt[[1]] > model.lm[[1]]) {
    final_model = append(final_model, list(model.lm[[2]]))
  } else {
    final_model = append(final_model, list(model.rt[[2]]))
  }
}

#predicting values of test sample
test.algae = fillNA(test.algae, 3)
final_prediction = data.frame(a1=integer(140),a2=integer(140),a3=integer(140),a4=integer(140),a5=integer(140),a6=integer(140),a7=integer(140))
for(i in 1:7) {
  final_prediction[,i] = predict(final_model[[i]], test.algae)
}

final_prediction - algae.sols
