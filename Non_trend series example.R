set.seed(1234)

ts_non_trend <- ts(runif(200, 5,5.2),
                   start = c(2000, 1),
                   frequency = 12)

ts_linear_trend_p <- ts_non_trend + 1:length(ts_non_trend) / 
  (0.5 * length(ts_non_trend))

ts_linear_trend_n <- ts_non_trend - 1:length(ts_non_trend) / 
  (0.5 * length(ts_non_trend))

ts_exp_trend <- ts_non_trend + exp((1:length(ts_non_trend) -1) / 
  (0.5 * length(ts_non_trend))) -1
