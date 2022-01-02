
### Do not alter.
library(parallel)
targets = c("log_vol", "mean_return")
###

### Experiment parameters
num_of_top_words = 2 # num_of_top_words must be >= 1
target = targets[1]
y_lags = 2 # y_lags = 0 means no lagged variables included
x_lags = 2 # x_lags = 0 means no lagged words included
           # max(x_lags, y_lags) must < 9
train_por = 0.7
mc_cores = 1
prll = FALSE
###

### Loading data ###############################################################

dta = read.csv(paste0("./final_data/dta_", 0, ".csv"))
if (num_of_top_words > 1) {
  for (i in 1:(num_of_top_words - 1)) {
    temp = read.csv(paste0("./final_data/dta_", i, ".csv"))
    dta = cbind(dta, temp[,-c(1:5)])
    rm(temp)
  }
}

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
lag_by_cik = function(cik, ciks, data, lags, cik_append = TRUE) {
  if (is.vector(data)) {
    sub_data = as.vector(data[ciks == cik])
  } else if (is.matrix(data)) {
    sub_data = as.matrix(data[ciks == cik,])
  } else {
    stop("lag_by_cik: data is neither vector nor matrix.")
  }
  if (cik_append) {
    return(cbind(cik, embed(sub_data, lags + 1)))
  } else {
    return(embed(sub_data, lags + 1))
  }
}


y_trans = lapply(unique(ciks), lag_by_cik, ciks = ciks, data = y, lags = lags)
X_trans = lapply(unique(ciks), lag_by_cik, ciks = ciks, data = X, 
                 lags = lags, cik_append = FALSE)

Y_aux = NULL
X_aux = vector(mode = "list", length = num_of_top_words)
if (y_lags > 0) {
  Y_lag = vector(mode = "list", length = y_lags)
}
if (x_lags > 0) {
  X_lag = vector(mode = "list", length = x_lags * num_of_top_words)
}
for (i in 1:length(y_trans)) {
  Y_aux = rbind(Y_aux, y_trans[[i]][,1:(1 + length(target))])
  for (j in 1:num_of_top_words) {
    X_aux[[j]] = rbind(X_aux[[j]],
                       X_trans[[i]][,((j - 1) * 200 + 1):(j * 200)])
  }
  if (y_lags > 0) {
    for (j in 1:y_lags) {
      temp = y_trans[[i]][,(2 + j * length(target)):(1 + (j + 1) * length(target))]
      Y_lag[[j]] = rbind(Y_lag[[j]], 
                         as.matrix(temp, ncol = length(target)))
    }
    rm(temp)
  }
  if (x_lags > 0) {
    for (j in 1:x_lags) {
      for (k in 1:num_of_top_words) {
        X_lag_index = (j - 1) * num_of_top_words + k
        X_lag_word_index = j * 200 * num_of_top_words + (k - 1) * 200 + 1
        X_lag[[X_lag_index]] = rbind(X_lag[[X_lag_index]],
                                     X_trans[[i]][,X_lag_word_index:(X_lag_word_index + 199)])
      }
    }
  }
}

### Train-test split ###########################################################

train_test_split_by_cik = function(cik, ciks, data, por) {
  if (is.vector(data)) {
    sub_data = as.vector(data[ciks == cik])
    tot_n = length(sub_data)
    return(list("cik" = cik, "training" = sub_data[1:floor(por * tot_n)],
                "test" = sub_data[(floor(por * tot_n) + 1):tot_n]))
  } else if (is.matrix(data)) {
    sub_data = as.matrix(data[ciks == cik,])
    tot_n = nrow(sub_data)
    return(list("cik" = cik, "training" = sub_data[1:floor(por * tot_n),],
                "test" = sub_data[(floor(por * tot_n) + 1):tot_n,]))
  } else {
    stop("train_test_split_by_cik: data is neither vector nor matrix.")
  }
}

Y_train = NULL
Y_test = NULL
X_train = vector(mode = "list", length = num_of_top_words)
X_test = vector(mode = "list", length = num_of_top_words)
if (x_lags > 0) {
  X_lag_train = vector(mode = "list", length = x_lags * num_of_top_words)
  X_lag_test = vector(mode = "list", length = x_lags * num_of_top_words)
}
if (y_lags > 0) {
  Y_lag_train = vector(mode = "list", length = y_lags)
  Y_lag_test = vector(mode = "list", length = y_lags)
}

ciks = Y_aux[,1]
y_trans = lapply(unique(ciks), train_test_split_by_cik, ciks = ciks,
                 data = Y_aux[,-1], por = train_por)

for (i in 1:length(y_trans)) {
  y_temp = y_trans[[i]]
  Y_train = rbind(Y_train,
                  as.matrix(y_temp$training, ncol = length(target)))
  Y_test = rbind(Y_test, 
                 as.matrix(y_temp$test, ncol = length(target)))
}
rm(y_temp)

for (i in 1:num_of_top_words) {
  X_trans = lapply(unique(ciks), train_test_split_by_cik, ciks = ciks,
                   data = X_aux[[i]], por = train_por)
  for (j in 1:length(X_trans)) {
    X_train[[i]] = rbind(X_train[[i]],
                         as.matrix(X_trans[[j]]$training))
    X_test[[i]] = rbind(X_test[[i]],
                        as.matrix(X_trans[[j]]$test))
  }
}

for (i in 1:y_lags) {
  y_trans = lapply(unique(ciks), train_test_split_by_cik, ciks = ciks,
                   data = Y_lag[[i]], por = train_por)
  for (j in 1:length(y_trans)) {
    Y_lag_train[[i]] = rbind(Y_lag_train[[i]],
                             as.matrix(y_trans[[j]]$training, ncol = length(target)))
    Y_lag_test[[i]] = rbind(Y_lag_test[[i]],
                            as.matrix(y_trans[[j]]$test, ncol = length(target)))
  }
}

for (i in 1:x_lags) {
  X_trans = lapply(unique(ciks), train_test_split_by_cik, ciks = ciks,
                   data = X_lag[[i]], por = train_por)
  for (j in 1:length(X_trans)) {
    X_lag_train[[i]] = rbind(X_lag_train[[i]],
                             X_trans[[j]]$training)
    X_lag_test[[i]] = rbind(X_lag_test[[i]],
                            X_trans[[j]]$test)
  }
}

rm(X_trans)
rm(y_trans)

cat("The training set contains", nrow(Y_train), "data points", "\n")
if (max(table(Y_train[,1])) != min(table(Y_train[,1]))) {
  stop("sample contains different year length for different companies.")
}
cat("It contians", length(unique(Y_train[,1])), "companies, over the horizon of",
    mean(table(Y_train[,1])), "years", "\n")

cat("The test set contains", nrow(Y_test), "data points", "\n")
if (max(table(Y_test[,1])) != min(table(Y_test[,1]))) {
  stop("sample contains different year length for different companies.")
}
cat("It contians", length(unique(Y_test[,1])), "companies, over the horizon of",
    mean(table(Y_test[,1])), "years", "\n")


# By now, we have the response matrix (vector) Y_train and Y_test
# as well as the lagged response matrices (vectors) Y_lag_tran and Y_lag_test
# and word embeddings X_train and X_test
# as well as their lagged versions X_lag_train and X_lag_test

### Train RGA ##################################################################

dims = c(length(target), rep(length(target), y_lags), 
         rep(rep(200, num_of_top_words), x_lags + 1))

features

model_RGA = rga()



