#' Pairwise permutation tests
#'
#' @description
#' Conducts all possible pairwise permutation tests between the groups in the 
#' data set given and a given reference group. 
#' 
#' * If requested, the Global permutation test is performed over the entire 
#' data set, and reported in the result data frame of this function. 
#' 
#' * If family correction method(s) are given, the base R p.adjust routine is
#'  called to adjust the p.value calculated from the pairwise permutation tests.
#'  
#' * Progress bars (from txtProgressBar) are shown in all instances, but a
#'  `verbose` option (TRUE/FALSE) controls output to the terminal after each 
#'  test. 
#' 
#' * Given that this function calls the global_permutation_test function on the 
#' backend (just with different input tables), the only other arguments needed 
#' (vs the global call) are the `compare_to`, `p_adj_meths` and 
#' `global_test_first`.
#'
#' @param data_ the data set, with at least three features: event (what 
#' happened), id (who were they), group (what intervention were they 
#' administered)
#' @param ... currently ignored
#' @param group_col_name the name of the column with `group` information 
#' @param id_col_name the name of the column with participant/patient `id` 
#' information
#' @param event_col_name the name fo the column with the event (data type 
#' castable to numerical)
#' @param ntrials the number of permutation tests performed (and test 
#' statistics calculated). currently capped at N!
#' @param parallel True/False whether the permutations/test statistics are to 
#' be calculated serially or in parallel (uses the R snow package) 
#' @param ranseed a set random seed to be used for the permutations (used for 
#' reproducibility)
#' @param systematic True/False whether the groups are to be grouped or 
#' distributed totally randomly. For example, should all observations from 
#' Patient A in group (G1) be permuted to the same group (G2), or can they be 
#' spread across permuted groups? The first option (TRUE) is endorsed by 
#' Eleanor, the second option (FALSE) would be in line with other R packages 
#' (`coin`, for example).
#' @param na_fill if any entries in the input table have empty groups, either 
#' (TRUE) fill them with a generated group name .NA_group, or (FALSE) filter 
#' those rows out.
#' @param compare_to for each pairwise test, the group that will be used for 
#' comparison
#' @param verbose TRUE/FALSE whether a completion message with stats should be 
#' printed to the screen whenever the permutation test ends. A progress bar is 
#' printed in all cases.
#' @param p_adj_meths a list of family corrections to be performed on the series 
#' of p-values obtained from the pairwise tests. Since this is a call to the 
#' base p.adjust method, the list of corrections can be found by running 
#' `?p.adjust.methods`. To summarise here:
#' * "bonferroni"
#'
#' * "holm" - Holm method,
#'
#' * "hochberg"
#'
#' * "hommel"
#'
#' * "BH"/"fdr" - Benjamini-Hochberg
#'
#' * "BY" - Benjamini-Yakutieli
#'
#' * "none" - a pass-through (i.e., no adjustment to the p values)
#'
#' @param global_test_first TRUE/FALSE whether a global permutation test 
#' should be performed on the data set before the full set of pairwise tests
#' 
#' @return A tibble (data frame) with the columns:
#' 
#'  * `group_name` the non-reference group in the pairwise test (for a global 
#'  test, "__GlobaL__"),
#'
#'  * `N_obs` the total number of observations used in each permutation tests 
#'  (that is, from all included groups),
#'
#'  * `N_trials` the number of permutations done
#'
#'  * `p` the p value obtained after performing the corresponding test
#'
#'  * `error` the Monte Carlo error term calculated for the test
#'
#'  * `compare_to` the group to be included in all pairwise tests (i.e., a 
#'  placebo group in a trial)
#'
#'  * `p_adj_...` the adjusted p value of the test, by each requested method 
#'  in `p_adj_meths`. The adjusted values form each test are given in different 
#'  columns, with the name "p_adj_" followed by the name of the method (from 
#'  p.adjust.methods)
#'
#' @importFrom plyr rbind.fill
#' @importFrom dplyr tibble as_tibble bind_rows
#' @importFrom stats p.adjust
#' @importFrom purrr reduce map
#' @importFrom data.table rbindlist
#'
#' @example examples/pairwise_permutation_tests_example.R
#'
#' @export
pairwise_permutation_tests <- function(data_,
                                       ...,
                                       group_col_name = "group_",
                                       id_col_name = "id_",
                                       event_col_name = "event_",
                                       ntrials = 100,
                                       parallel = FALSE,
                                       ranseed = NaN,
                                       systematic = TRUE,
                                       na_fill = FALSE,
                                       backsolve = FALSE,
                                       compare_to = "placebo",
                                       p_adj_meths = c("BH"),
                                       global_test_first = TRUE,
                                       verbose = TRUE) {

  data_groups <- unique(data_[[group_col_name]])
  if (! compare_to %in% data_groups) {
    stop(sprintf(
      paste("Requested reference group `%s` is not among the groups",
            "in the %s data frame. Aborting pairwise permutation test."),
      compare_to, deparse(substitute(data_))
    ))
  }
  other_groups <- data_groups %>% .[. != compare_to] %>% unique

  pairwise_test_results <- list()

  standard_data <- standard_table(data_,
                                  group_col_name = group_col_name,
                                  id_col_name = id_col_name,
                                  event_col_name = event_col_name,
                                  na_fill = na_fill)

  nested_list_to_tibble <- function(dat) {
    return(dat %>% map(as_tibble) %>% reduce(bind_rows))
  }

  gt_results <- NA

  if (global_test_first) {

    # no further arguments needed with the standardised table
    gt_results <- global_permutation_test(
      standard_data,
      ntrials = ntrials,
      parallel = parallel,
      ranseed = ranseed,
      systematic = systematic,
      verbose = TRUE
    )
  }

  for (comparing_to in other_groups) {

    pairwise_test_results[[comparing_to]] <- global_permutation_test(
      # janky code to do the group filtering based on the unique name given
      # will change
      data_ %>%
        rename(temp_group = !!group_col_name) %>%
        subset(temp_group %in% c(comparing_to, compare_to)) %>%
        rename(!!group_col_name := !!"temp_group"),
      group_col_name = group_col_name,
      id_col_name = id_col_name,
      event_col_name = event_col_name,
      ntrials = ntrials,
      parallel = parallel,
      ranseed = ranseed,
      systematic = systematic,
      na_fill = na_fill,
      verbose = verbose
    )
  }

  final_res <- pairwise_test_results %>%
    map(as_tibble) %>%
    reduce(bind_rows) %>%
    mutate(
      group_name = other_groups,
      compare_to = compare_to
    )

  family_corrections <- p_adj_meths %>%
    .[. %in% p.adjust.methods & . != "none"]


  if (length(family_corrections) > 0) {

    final_res <- sapply(
      unique(p_adj_meths),
      \(x) p.adjust(final_res$p, eval(x))
    ) %>%
      as.data.frame %>%
      tibble %>%
      rename_with(~paste0("p_adj_", .x)) %>%
      bind_cols(final_res)
  }

  if (is.list(gt_results)) {

    final_res <- gt_results %>%
      list() %>%
      nested_list_to_tibble %>%
      mutate(
        group_name = "__Global__",
        compare_to = as.character(NA)
      ) %>%
      bind_rows(final_res)
  }

  final_res <- final_res %>%
    relocate(group_name, N_obs, N_trials, p, error)

  return(final_res)

}