
### Do not alter.
library(parallel)
targets = c("log_vol", "mean_return")
###

### Experiment parameters
num_of_top_words = 1
target = targets
y_lags = 2 # y_lags = 0 means no lagged variables included
x_lags = 1 # x_lags = 0 means no lagged words included
           # max(x_lags, y_lags) must < 9
train_por = 0.7
mc_cores = 1
prll = FALSE
###

### Loading data ###############################################################

dta = read.csv(paste0("./final_data/dta_", 0, ".csv"))
for (i in 1:num_of_top_words) {
  temp = read.csv(paste0("./final_data/dta_", i, ".csv"))
  dta = cbind(dta, temp[,-c(1:5)])
  rm(temp)
}

dims = c(length(target), rep(length(target), y_lags), 
         rep(rep(200, num_of_top_words), x_lags + 1))

if (length(target) == 2) {
  y = as.matrix(dta[,4:5])
} else if (target == "log_vol") {
  y = as.vector(dta[,4])
} else if (target == "mean_return") {
  y = as.vector(dta[,5])
} else {
  stop("target is not defined. Must be either log_vol or mean_return, or both.")
}

X = as.matrix(dta[,-c(1:5)])

lags = max(y_lags, x_lags)
if (lags >= 9) {
  stop("too many lags used.")
}

### Taking lags ################################################################

ciks = dta[,1]
lag_by_cik = function(cik, ciks, data, lags) {
  if (is.vector(data)) {
    sub_data = as.vector(data[ciks == cik])
  } else if (is.matrix(data)) {
    sub_data = as.matrix(data[ciks == cik,])
  } else {
    stop("lag_by_cik: data is neither vector nor matrix.")
  }
  
  return(cbind(cik, embed(sub_data, lags + 1)))
}


y_trans = lapply(unique(ciks), lag_by_cik, ciks = ciks, data = y, lags = lags)
X_trans = lapply(unique(ciks), lag_by_cik, ciks = ciks, data = X, lags = lags)

Y_aux = NULL
X_aux = NULL
if (y_lags > 0) {
  Y_lag = vector(mode = "list", length = y_lags)
}
if (x_lags > 0) {
  X_lag = vector(mode = "list", length = x_lags)
}
for (i in 1:length(y_trans)) {
  Y_aux = rbind(Y_aux, y_trans[[i]][,1:(1 + length(target))])
  X_aux = rbind(X_aux, X_trans[[i]][,1:(1 + 200)])
  if (y_lags > 0) {
    for (j in 1:y_lags) {
      Y_lag[[j]] = rbind(Y_lag[[j]], y_trans[[i]][,(2 + j * length(target)):(1 + (j + 1) * length(target))])
    }
  }
  if (x_lags > 0) {
    for (j in 1:x_lags) {
      X_lag[[j]] = rbind(X_lag[[j]], X_trans[[i]][,(2 + j * 200):(1 + (j + 1) * 200)])
    }
  }
}

### Train-test split ###########################################################

train_test_split_by_cik = function(cik, ciks, data, por) {
  if (is.vector(data)) {
    sub_data = as.vector(data[ciks == cik])
    tot_n = length(sub_data)
  } else if (is.matrix(data)) {
    sub_data = as.matrix(data[ciks == cik,])
    tot_n = nrow(sub_data)
  } else {
    stop("train_test_split_by_cik: data is neither vector nor matrix.")
  }
  
  return(list("cik" = cik, "training" = sub_data[1:floor(por * tot_n),],
              "test" = sub_data[(floor(por * tot_n) + 1):tot_n,]))
}

ciks = Y_aux[,1]
y_trans = lapply(unique(ciks), train_test_split_by_cik, ciks = ciks,
                 data = Y_aux[,-1], por = train_por)
X_trans = lapply(unique(ciks), train_test_split_by_cik, ciks = ciks,
                 data = X_aux[,-1], por = train_por)

Y_train = NULL
X_train = NULL

Y_test = NULL
X_test = NULL
for (i in 1:length(y_trans)) {
  y_temp = y_trans[[i]]
  X_temp = X_trans[[i]]
  Y_train = rbind(Y_train,
                  as.matrix(cbind(y_temp$cik, y_temp$training)))
  Y_test = rbind(Y_test, 
                 as.matrix(cbind(y_temp$cik, y_temp$test)))
  
  X_train = rbind(X_train,
                  as.matrix(cbind(X_temp$cik, X_temp$training)))
  X_test = rbind(X_test,
                 as.matrix(cbind(X_temp$cik, X_temp$test)))
}
rm(y_temp)
rm(X_temp)

if (y_lags > 0) {
  Y_lag_train = vector(mode = "list", length = y_lags)
  Y_lag_test = vector(mode = "list", length = y_lags)
  for (j in 1:y_lags) {
    y_trans = lapply(unique(ciks), train_test_split_by_cik, ciks = ciks,
                     data = Y_lag[[j]], por = train_por)
    for (i in 1:length(y_trans)) {
      temp = y_trans[[i]]
      Y_lag_train[[j]] = rbind(Y_lag_train[[j]],
                               as.matrix(cbind(temp$cik, temp$training)))
      Y_lag_test[[j]] = rbind(Y_lag_test[[j]],
                              as.matrix(cbind(temp$cik, temp$test)))
    }
  }
  rm(temp)
}
if (x_lags > 0) {
  X_lag_train = vector(mode = "list", length = x_lags)
  X_lag_test = vector(mode = "list", length = x_lags)
  for (j in 1:x_lags) {
    X_trans = lapply(unique(ciks), train_test_split_by_cik, ciks = ciks,
                     data = X_lag[[j]], por = train_por)
    for (i in 1:length(X_trans)) {
      temp = X_trans[[i]]
      X_lag_train[[j]] = rbind(X_lag_train[[j]],
                               as.matrix(cbind(temp$cik, temp$training)))
      X_lag_test[[j]] = rbind(X_lag_test[[j]],
                              as.matrix(cbind(temp$cik, temp$test)))
    }
  }
  rm(temp)
}

### Train RGA ##################################################################





