#some standard info about data
summary(algae)
hist(algae$mxPH)

#getting two correlating vars that can fill in each other
symnum(cor(algae[, 4:18], use="complete.obs"))

#define function that find the most polluted (with NA) rows
lazy_clean = function(df, p=0) {
  l = list()
  n = nrow(df)
  m = ncol(df)
  for (i in 1:n) {
    nas = sum(is.na(df[i,]))
    if(nas/m > p) {
      l = append(l, i)
    }
  }
  unlist(l)
}

algae_clean = algae[-lazy_clean(algae, 0.2),]

#linear regression
coeffs = lm(PO4 ~ oPO4, data = algae_clean)$coefficients
#filling out some NAs
for (i in lazy_clean(algae)) {
  if (!is.na(algae[i, "oPO4"])) {
    algae_clean[i, "PO4"] = coeffs[1] + coeffs[2] * algae[i, "oPO4"]
  }
  if (!is.na(algae[i, "PO4"])) {
    algae_clean[i, "oPO4"] = -coeffs[1]/coeffs[2] + 1/coeffs[2] * algae[i, "PO4"]
  }
  
}
summary(algae_clean)


#metric function
metric = function(x, y) {
  sqrt(sum((x - y)^2))
}

#k nearest neighbors
knn = function(t, df, k) {
   neighbors = integer(k)
   n = nrow(df)
   for (i in 1:n) {
     if(t != i) {
       for(j in 1:k) {
         if (neighbors[j] == 0 && sum(is.na(df[i,])) == 0) {
           neighbors[j] = i
           break
         }
         if(sum(is.na(df[i,])) == 0) {
           if(!(i %in% neighbors) && metric(df[t,], df[i,]) < metric(df[t,], df[neighbors[j],])) {
             neighbors[j] = i
           }
         }
       }
     }
   }
   neighbors
}

mean_df = function(df) {
  result = list()
  n = ncol(df)
  if(!is.null(n)) {
    for(i in 1:n) {
      result = append(result, mean(df[,i]))
    }
  } else {
    result = mean(df)
  }
  result
}

#fill out nas with knn average
fillNA = function(df, k) {
  ind = lazy_clean(df)
  print("Places that needs to be filled out")
  print(ind)
  for (i in ind) {
    ind_na = lazy_clean(t(df[i,]))
    temp_df = df[,-ind_na]
    close_entries = knn(i, temp_df, k)
    avg_vector = mean_df(df[close_entries, ind_na])
    df[i, ind_na] = avg_vector
  }
  print("Filled out")
  df
}

algae_clean[,4:18] = fillNA(algae_clean[,4:18], 4)
algae[,-is.na(algae_clean[199, 4:18])]
