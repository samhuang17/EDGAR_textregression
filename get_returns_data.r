library(attempt)

dta = read.csv("./top_w2v/top_0.csv", header = FALSE)

one_true = function(logic_vec) {
  for (x in logic_vec) {
    if (x) {
      return(TRUE)
    }
  }
  return(FALSE)
}

### Get stock return data via Tidyquant package

tickers = unique(dta$V2)
final_dta = NULL
failed_tickers = NULL
pos = 1

from = "2010-01-01"
to = "2019-12-31"

for (ticker in tickers) {
  returns = try_catch(get_returns(ticker, from, to, period = "daily"),
                      .e = ~ NULL)
  temp = NULL
  if (one_true(returns[1] == ticker)) {
    for (yr in 2010:2019) {
      year_returns = (returns$daily.returns)[year(returns$date) == yr] 
      temp = c(ticker, yr, log(var(year_returns)), mean(year_returns))
      final_dta = rbind(final_dta, temp)
    }
    cat(pos, "/", length(tickers), " tickers completed", "\n")
  } else {
    cat(paste0("ticker failed: ", ticker, "\n"))
    failed_tickers = c(failed_tickers, ticker)
  }
  pos = pos + 1
}

### Catch more failed tickers
abnorm = function(sub_dta) {
  if (one_true(sub_dta[,4] == "0")) {
    return(sub_dta[1,1])
  } else if (one_true(is.na(sub_dta[,3]))) {
    return(sub_dta[1,1])
  } else {
    return(0)
  }
  
}

for (ticker in unique(final_dta[,1])) {
  sub_dta = final_dta[final_dta[,1] == ticker,]
  res = abnorm(sub_dta)
  if (res == 0) {
    next
  } else {
    failed_tickers = c(failed_tickers, res)
  }
}

final_dta = final_dta[!(final_dta[,1] %in% c(failed_tickers)),]

### Glue with dta

final_tickers = unique(final_dta[,1])
dta = dta[dta$V2 %in% final_tickers,]

res = cbind(dta[,1:3], as.numeric(final_dta[,3]), as.numeric(final_dta[,4]),
            dta[,4:203])

length(unique(res[,2])) # 1,927 companies
dim(res)[1] # 19,270 observations

names(res) = c("CIK", "ticker", "year", "log_vol", "mean_return", 
               paste0("V", 1:200))

write.csv(res, "./final_data/dta_0.csv", row.names = FALSE )

### clean other embedding data

for (i in 1:14) {
  new_dta = read.csv( paste0("./top_w2v/top_", i, ".csv"), header = FALSE)
  new_dta = new_dta[new_dta[,2] %in% final_tickers,]
  print(dim(new_dta))
  new_dta = cbind(new_dta[,1:3], as.numeric(final_dta[,3]), 
                  as.numeric(final_dta[,4]), new_dta[,4:203])
  names(new_dta) = c("CIK", "ticker", "year", "log_vol", "mean_return", 
                     paste0("V", (i * 200 + 1:200)))
  write.csv(new_dta, paste0("./final_data/dta_", i, ".csv"), row.names = FALSE)
}



