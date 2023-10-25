#' Permutation test helper (deprecated, please use `get_p_value`)
#' 
#' @example examples/get_p_value_example.R
#'
#' @export
permgp_fn <- function(data_,
                      ...,
                      group_col_name = "group_",
                      id_col_name = "id_",
                      event_col_name = "event_",
                      systematic = FALSE,
                      na_fill = TRUE,
                      ranseed = NA) {

  warning(paste(
    "Function 'pergp_fn' is deprecated and will soon be removed.",
    "Replace function name with 'get_p_value', with no other change to the",
    "function signature. Please see documentation for an added `na_fill`",
    "parameter."
  ))

  temp <- get_p_value(data_,
                      group_col_name = group_col_name,
                      id_col_name = id_col_name,
                      event_col_name = event_col_name,
                      systematic = systematic,
                      na_fill = na_fill,
                      ranseed = ranseed)

  return(temp)
}