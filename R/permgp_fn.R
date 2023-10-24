#' Permutation test helper (deprecated, please use `get_p_value`)
#'
#' Standardises the input data table and retrieves the test statistic (p value)
#'
#' @param data_ data frame with at least three columns: group, id and event
#' @param group_col_name name of the `group` column
#' @param id_col_name name of the `id` column
#' @param systematic whether the groups should be permuted consistently with 
#' the id, or not
#'
#' @returns The test statistic after even groups have been permuted
#'
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select mutate right_join tibble join_by all_of .data
#'
#' @example examples/permgp_fn_example.R
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
    "function signature. Please see documentation for an added `na_fill` ",
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