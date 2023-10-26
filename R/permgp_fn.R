#' Permutation test helper (deprecated, please use `single_permutation`)
#' 
#' @example examples/single_permutation_example.R
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
    "Replace function name with 'single_permutation', with no other change to the",
    "function signature. Please see documentation for an added `na_fill`",
    "parameter."
  ))

  temp <- single_permutation(data_,
                      group_col_name = group_col_name,
                      id_col_name = id_col_name,
                      event_col_name = event_col_name,
                      systematic = systematic,
                      na_fill = na_fill,
                      ranseed = ranseed)

  return(temp)
}