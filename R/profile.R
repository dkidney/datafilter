#' @title Column profiler
#' @description
#' A short description...
#' @param data a data.frame or tibble
#' @param p_mode_min todo
#' @param p_mode_max_card todo
#' @param flag_high_card_cat todo
#' @param flag_low_card_num todo
#' @returns A tibble with rows representing the columns of \code{data} and columns representing
#' @export
#'
#' @examples
#' (example_data = load_example_data())
#' (example_data_profile = example_data |> profile_columns())
#' example_data |> drop_columns(example_data_profile, dryrun=TRUE)
#'
#' (mtcars_profile = mtcars |> profile_columns())
#' mtcars |> drop_columns(mtcars_profile, dryrun=TRUE)
profile_columns = function(data,
                           p_mode_min = 0.5,
                           p_mode_max_card = 30,
                           flag_high_card_cat = 30,
                           flag_low_card_num = 10) {
  n = nrow(data)
  p = ncol(data)
  types = data |> map_chr(type_sum)
  n_na = data |> map_int(~sum(is.na(.x)))
  n_not_na = n - n_na
  p_na = n_na / n
  n_unique = data |> map_int(n_distinct, na.rm=TRUE)
  p_unique = n_unique / (n - n_na)

  # mode -----------------------------------------------------------------------
  # browser()
  v_mode = rep(NA, p)
  n_mode = rep(NA_integer_, p)
  # limit by cardinality
  i = n_unique <= p_mode_max_card
  modes = data[,i] |> map(calc_n_mode4)
  v_mode[i] = modes |> map('mode') |> unlist()
  n_mode[i] = modes |> map_int('n_mode')
  # limit by max p_mode
  max_possible_n_mode = n_not_na - n_unique + 1
  # j = max_possible_n_mode == 2
  # n_mode[j] = 2
  max_possible_p_mode = max_possible_n_mode / n
  k = max_possible_p_mode > p_mode_min
  k = k & !i
  modes = data[,k] |> map(calc_n_mode4)
  v_mode[k] = modes |> map('mode') |> unlist()
  n_mode[k] = modes |> map_int('n_mode')
  # modes = data[,!(i|j|k)] |> map(calc_n_mode4)
  # v_mode[!(i|j|k)] = modes |> map('mode') |> unlist()
  # n_mode[!(i|j|k)] = modes |> map_int('n_mode')

  # var -----------------------------------------------------------------------
  vars = rep(NA, p)
  # var numeric
  i = types %in% c('dbl', 'int')
  vars[i] = data[,i] |> map_dbl(var, na.rm=TRUE)
  # var binary
  i = n_unique == 2
  n1 = n_mode[i]
  n2 = n_not_na[i] - n1
  vars[i] = n1 / n_not_na[i] * n2 / n_not_na[i]

  # high_card_cat
  high_card_cat = rep(NA, p)
  i = types %in% c('chr', 'fct', 'ord')
  j = n_unique >= flag_high_card_cat
  high_card_cat[i & j] = TRUE
  high_card_cat[i & !j] = FALSE

  # low_card_num
  low_card_num = rep(NA, p)
  i = types %in% c('dbl', 'int')
  j = n_unique <= flag_low_card_num
  low_card_num[i & j] = TRUE
  low_card_num[i & !j] = FALSE

  tibble(
    colname = data |> colnames(),
    type = !!types,
    n_na = !!n_na,
    p_na = !!p_na,
    n_unique = !!n_unique,
    p_unique = !!p_unique,
    mode = !!v_mode,
    p_mode = !!n_mode / !!n,
    var = !!vars,
    high_card_cat = !!high_card_cat,
    low_card_num = !!low_card_num
  )
}

calc_n_mode = function(x) {
  tab = table(x, useNA = 'no')
  unname(tab[which.max(tab)])
}

calc_n_mode2 = function(x) {
  ux <- unique(x)
  max(tabulate(match(x, ux)))
}

calc_n_mode3 = function(x, p_mode_limit = 0.5) {
  n_not_na = sum(!is.na(x))
  max_possible_n_mode = n_not_na - n_distinct(x) + 1
  max_possible_p_mode = max_possible_n_mode / length(x)
  if (max_possible_p_mode < p_mode_limit) return(NA_integer_)
  ux <- unique(x)
  max(tabulate(match(x, ux)))
}

calc_n_mode4 = function(x) {
  ux <- unique(x)
  tab = tabulate(match(x, ux))
  i = which.max(tab)
  list(
    mode = ux[i],
    n_mode = tab[i]
  )
}

drop_rows = function(data, dryrun=FALSE) {
  rows_to_drop = data |> apply(1, function(x) all(is.na(x)))
  n_rows_to_drop = sum(rows_to_drop)
  if (n_rows_to_drop == 0) {
    message(n_rows_to_drop, ' empty rows')
  }
  if (dryrun) {
    return(invisible(which(rows_to_drop)))
  }
  data |> slice(rows_to_drop)
}

#' Title
#'
#' @param data todo
#' @param profile todo
#' @param dryrun todo
#' @param min_n_unique todo
#' @param max_p_na todo
#' @param min_var_numeric todo
#' @param min_var_binary todo
#' @param max_p_mode todo
#' @param ... todo
#'
#' @returns
#' @export
#'
#' @examples
#' # todo
drop_columns = function(data,
                        profile=NULL,
                        dryrun=TRUE,
                        min_n_unique = 2,
                        max_p_na = 0.9,
                        min_var_numeric=1e-8,
                        min_var_binary=0.01,
                        max_p_mode=0.99,
                        ...) {
  if (is.null(profile)) {
    message('generating column profile')
    profile = profile_columns(data, ...)
  }

  cols_to_drop = NA_character_

  cols = profile |>
    filter(.data$n_unique < !!min_n_unique) |>
    pull(.data$colname)
  cols = cols |> setdiff(cols_to_drop)
  if (length(cols) > 0) message('droping ', length(cols), ' columns with n_unique < ', min_n_unique)
  cols_to_drop = c(cols_to_drop, cols)

  cols = profile |>
    filter(.data$p_na > !!max_p_na) |>
    pull(.data$colname)
  cols = cols |> setdiff(cols_to_drop)
  if (length(cols) > 0) message('droping ', length(cols), ' columns with p_na > ', max_p_na)
  cols_to_drop = c(cols_to_drop, cols)

  cols = profile |>
    filter(.data$type %in% c('dbl', 'int')) |>
    filter(.data$var < !!min_var_numeric) |>
    pull(.data$colname)
  cols = cols |> setdiff(cols_to_drop)
  if (length(cols) > 0) message('droping ', length(cols), ' columns with var (numeric) < ', min_var_numeric)
  cols_to_drop = c(cols_to_drop, cols)

  cols = profile |>
    filter(.data$n_unique == 2) |>
    filter(.data$var < !!min_var_binary) |>
    pull(.data$colname)
  cols = cols |> setdiff(cols_to_drop)
  if (length(cols) > 0) message('droping ', length(cols), ' columns with var (binary) < ', min_var_binary)
  cols_to_drop = c(cols_to_drop, cols)

  cols = profile |>
    filter(.data$p_mode > !!max_p_mode) |>
    pull(.data$colname)
  cols = cols |> setdiff(cols_to_drop)
  if (length(cols) > 0) message('droping ', length(cols), ' columns with p_mode > ', max_p_mode)
  cols_to_drop = c(cols_to_drop, cols)

  n_high_card_cat = sum(profile$high_card_cat, na.rm = TRUE)
  if (n_high_card_cat > 0) warning(n_high_card_cat, ' columns with high_card_cat')

  n_low_card_num = sum(profile$low_card_num, na.rm = TRUE)
  if (n_low_card_num > 0) warning(n_low_card_num, ' columns with low_card_num')

  if (dryrun) return(invisible(data))

  data |> select(-one_of(cols_to_drop))

}


#' Title
#'
#' @returns todo
#' @export
#'
#' @examples
#' load_example_data()
load_example_data = function() {
  n = 100
  tibble(
    a = rep(1, n),
    b = rep(NA, n),
    c = rnorm(n),
    d = rnorm(n, sd=1e-5),
    e = c(1:3, rep(NA, n-3)),
    f = c(letters[1:3], rep(NA, n-3)),
    g = rep(1:10, times=10),
    h = rep(letters[1:10], times=10),
    i = as.factor(.data$h),
    j = c(1, rep(0, n-1)),
  )
}
