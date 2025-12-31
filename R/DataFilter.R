# @title R6 Class for data frame filtering
# @description todo
# @export
# @examples
#
# data = load_example_data()
# df = DataFilter$new(data)
# df$profile
# df$drop(data) |> head()
#
# data = mtcars
# df = DataFilter$new(data)
# df$profile
# df$drop(data) |> head()
#
# data = airquality
# df = DataFilter$new(data)
# df$profile
# df$drop(data) |> head()
DataFilter = R6::R6Class(
  "DataFilter",
  clone=FALSE,
  public = list(

    # @field profile column profile
    profile = NULL,

    # @description todo
    # @param data todo
    initialize = function(data) {
      # message('hello - this is initialize')
      data |> check_for_empty_rows(drop_rows=FALSE)
      self$profile = profile_columns(data)
    },

    # @description todo
    # @param data todo
    # @param min_n_unique todo
    # @param max_p_na todo
    # @param min_var_numeric todo
    # @param min_var_binary todo
    # @param max_p_mode todo
    # @param drop_empty_rows todo
    # @returns todo
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
