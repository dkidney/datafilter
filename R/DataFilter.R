#' @title R6 Class for data frame filtering
#' @description todo
#' @export
#' @examples
#'
#' data = load_example_data()
#' df = DataFilter$new(data)
#' df$profile
#' df$drop(data) |> head()
#'
#' data = mtcars
#' df = DataFilter$new(data)
#' df$profile
#' df$drop(data) |> head()
#'
#' data = airquality
#' df = DataFilter$new(data)
#' df$profile
#' df$drop(data) |> head()
DataFilter = R6::R6Class(
  "DataFilter",
  clone=FALSE,
  public = list(

    #' @field profile column profile
    profile = NULL,

    #' @description todo
    #' @param data todo
    initialize = function(data) {
      # message('hello - this is initialize')
      data |> check_for_empty_rows(drop_rows=FALSE)
      self$profile = profile_columns(data)
    },

    #' @description todo
    #' @param data todo
    #' @param min_n_unique todo
    #' @param max_p_na todo
    #' @param min_var_numeric todo
    #' @param min_var_binary todo
    #' @param max_p_mode todo
    #' @param drop_empty_rows todo
    #' @returns todo
    drop = function(data,
                    min_n_unique = 2,
                    max_p_na = 0.9,
                    min_var_numeric=1e-8,
                    min_var_binary=0.01,
                    max_p_mode=0.99,
                    drop_empty_rows=TRUE) {

      cols_union = c()

      cols = private$n_unique(min_n_unique)
      cols = cols |> setdiff(cols_union)
      if (length(cols) > 0) message('droping ', length(cols), ' columns with n_unique < ', min_n_unique)
      cols_union = c(cols_union, cols)

      cols = private$p_na(max_p_na)
      cols = cols |> setdiff(cols_union)
      if (length(cols) > 0) message('droping ', length(cols), ' columns with p_na > ', max_p_na)
      cols_union = c(cols_union, cols)

      cols = private$var_numeric(min_var_numeric)
      cols = cols |> setdiff(cols_union)
      if (length(cols) > 0) message('droping ', length(cols), ' columns with var (numeric) < ', min_var_numeric)
      cols_union = c(cols_union, cols)

      cols = private$var_binary(min_var_binary)
      cols = cols |> setdiff(cols_union)
      if (length(cols) > 0) message('droping ', length(cols), ' columns with var (binary) < ', min_var_binary)
      cols_union = c(cols_union, cols)

      cols = private$p_mode(max_p_mode)
      cols = cols |> setdiff(cols_union)
      if (length(cols) > 0) message('droping ', length(cols), ' columns with p_mode > ', max_p_mode)
      cols_union = c(cols_union, cols)

      n_high_card_cat = sum(self$profile$high_card_cat, na.rm = TRUE)
      if (n_high_card_cat > 0) warning(n_high_card_cat, ' columns with high_card_cat')

      n_low_card_num = sum(self$profile$low_card_num, na.rm = TRUE)
      if (n_low_card_num > 0) warning(n_low_card_num, ' columns with low_card_num')

      data |>
        select(-one_of(cols_union)) |>
        check_for_empty_rows(drop_rows=drop_empty_rows)
    }

  ),

  private = list(

    n_unique = function(min_n_unique) {
      self$profile |>
        filter(n_unique < !!min_n_unique) |>
        pull(colname)
    },

    p_na = function(max_p_na) {
      self$profile |>
        filter(p_na > !!max_p_na) |>
        pull(colname)
    },

    var_numeric = function(min_var_numeric) {
      self$profile |>
        filter(type %in% c('dbl', 'int')) |>
        filter(var < !!min_var_numeric) |>
        pull(colname)
    },

    var_binary = function(min_var_binary) {
      self$profile |>
        filter(n_unique == 2) |>
        filter(var < !!min_var_binary) |>
        pull(colname)
    },

    p_mode = function(max_p_mode) {
      self$profile |>
        filter(p_mode > !!max_p_mode) |>
        pull(colname)
    }

  )
)


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

profile_columns = function(data,
                           p_mode_limit = 0.5,
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

  n_mode = rep(NA, p)
  i = n_unique == n_not_na
  n_mode[i] = 1
  max_possible_n_mode = n_not_na - n_unique + 1
  j = max_possible_n_mode == 2
  n_mode[j] = 2
  max_possible_p_mode = max_possible_n_mode / n
  k = max_possible_p_mode > p_mode_limit
  n_mode[!(i|j|k)] = data[,!(i|j|k)] |> map_int(calc_n_mode2)
  p_mode = n_mode / n

  # var
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
    p_mode = !!p_mode,
    var = !!vars,
    high_card_cat = !!high_card_cat,
    low_card_num = !!low_card_num
  )
}

check_for_empty_rows = function(data, drop_rows=FALSE) {
  empty_rows = data |> apply(1, function(x) all(is.na(x)))
  n_empty_rows = sum(empty_rows)
  if (n_empty_rows == 0) {
    return(data)
  }
  if (drop_rows) {
    message('dropping ', n_empty_rows, ' empty rows')
    return(data[!empty_rows, , drop=FALSE])
  }
  warning('data contains ', n_empty_rows, ' empty rows')
  data
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
