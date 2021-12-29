library(tidyquant)
library(tidyverse)
library(ggplot2)

get_returns <- function(stock_symbol, from, to, period = "monthly") {
  stock_symbol %>%
    tq_get(get = "stock.prices",
           from = from, 
           to = to) %>%
      group_by(symbol) %>%
        tq_transmute(select = adjusted,
                     mutate_fun = periodReturn,
                     type = "arithmetic",
                     period = period)
}

### Test
from = "2012-01-01"
to = "2016-12-31"
stock_symbols = c("FB", "AAPL", "GOOGL", "AMZN", "TSLA", "F")

returns = get_returns(stock_symbols, from, to, period = "weekly")

returns %>%
  ggplot(aes(x = date, y = weekly.returns, group = symbol, color = symbol)) +
  geom_line() + 
  labs(title = "Tech stock returns", y = "weekly returns", x = "", color = "") +
  theme_tq() +
  theme(legend.position = "right")






