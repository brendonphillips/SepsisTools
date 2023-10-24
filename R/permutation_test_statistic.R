#' Test statistic calculation
#'
#' Calculate the test statistic (sum of square differences) between group means
#' and the overall mean
#'
#' @param events The scores to be parsed
#' @param groups The groups that the scores belong to
#'
#' @returns The sum of square differences between group means and the overall
#' event mean
#'
#' @example examples/permutation_test_statistic_example.R
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble mutate group_by summarise ungroup pull n
#'
#' @export
perm_test_statistic <- function(events, groups) {

  loc_events <- events
  loc_groups <- groups

  if (length(loc_groups) != length(loc_events)) {
    loc_groups <- rep(loc_groups, length.out = length(loc_events))
  }

  test_stat <- tibble(event = events, group = groups) %>%
    mutate(all_mean = sum(event) / nrow(.)) %>%
    group_by(group) %>%
    summarise(
      gp_mean = sum(event) / n(),
      all_mean = unique(all_mean)
    ) %>%
    ungroup() %>%
    summarise(final_res = sum((gp_mean - all_mean)**2)) %>%
    pull(final_res)

  return(test_stat)
}
