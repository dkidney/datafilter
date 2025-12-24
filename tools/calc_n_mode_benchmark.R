summarise_results = function(res, unit="us") {
  s = summary(res, unit=unit)
  data.frame(name = s$expr, median = s$median)
}

x = c(1, 1:3)
microbenchmark::microbenchmark(
  calc_n_mode(x),
  calc_n_mode2(x),
  calc_n_mode3(x, p_mode_limit=0.5),
  times=100L
) |> summarise_results()


x = c(1, 1:1e4)
microbenchmark::microbenchmark(
  calc_n_mode(x),
  calc_n_mode2(x),
  calc_n_mode3(x, p_mode_limit=0.5),
  times=100L
) |> summarise_results()


x = rnorm(n, 1e4)
microbenchmark::microbenchmark(
  calc_n_mode(x),
  calc_n_mode2(x),
  calc_n_mode3(x, p_mode_limit=0.5),
  times=100L
) |> summarise_results()
