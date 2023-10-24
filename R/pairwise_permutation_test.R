#' Pairwise permutation tests
#'
#' Conduct all possible permutation tests
#'
#' @param first lorem
#' @param second ipsum
#'
#' @return returning p values from pairwise tests
#'
#' @importFrom plyr rbind.fill
#' @importFrom dplyr tibble as_tibble bind_rows
#' @importFrom stats p.adjust
#' @importFrom purrr reduce map
#' @importFrom data.table rbindlist
#'
#' @examples "coming soon"
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
                                       reference_group = "placebo",
                                       step_down_procedures = c("BH"),
                                       global_test_first = TRUE,
                                       verbose = TRUE) {

  data_groups <- unique(data_[[group_col_name]])
  if (! reference_group %in% data_groups) {
    stop(sprintf(
      paste("Requested reference group `%s` is not among the groups",
            "in the %s data frame. Aborting pairwise permutation test."),
      reference_group, deparse(substitute(data_))
    ))
  }
  other_groups <- data_groups %>% .[. != reference_group] %>% unique

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
        subset(temp_group %in% c(comparing_to, reference_group)) %>%
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
      reference_group = reference_group
    )

  family_corrections <- step_down_procedures %>%
    .[. %in% p.adjust.methods & . != "none"]


  if (length(family_corrections) > 0) {

    final_res <- sapply(
      unique(step_down_procedures),
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
        group_name = "Global",
        reference_group = as.character(NA)
      ) %>%
      bind_rows(final_res)
  }

  final_res <- final_res %>%
    relocate(group_name, N_obs, N_trials, p, error)

  return(final_res)

}