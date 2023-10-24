#' Global Permutation Test
#'
#' Carry through the permutation test
#'
#' @param data_ data frame with at least 3 columns: event (yes/no/numerical describing what happened), id (unique id of the patient/participant), group (what trial/observation group they belonged to)
#' @param group_col_name the name of the (IP) column
#' @param id_col_name name of the (participant) id column
#' @param event_col_name name of the event column
#' @param ntrials the number of permutation tests in the ensemble
#' @param parallel True/False whether the permutations should be run in
#' parallel for speed
#' @param ranseed possibility for using a fixed random seed, for reproducibility
#' @param systematic True/False whether the groups are to be grouped or distributed totally randomly. For example, should all observations from Patient A in group (G1) be permuted to the same group (G2), or can they be sporead across permuted groups? The second would be in line with other prepackaged R libraries.
#' @param na_fill if any entries in the input table have empty groups, fill
#' them with a generated group name (.NA_group), or filter those rows out
#' @param verbose TRUE/FALSE whether a completion message with stats should be
#' printed to the screen whenever the permutation test ends. A progress bar is
#' printed in all cases.
#'
#' @return A list giving `$p` (the p-value) and `$error` (the Monte-Carlo
#' error) of the calculation, `$N_perms` the number of permutations performed/test statistics calculated, `$N_trials` the total number of observations for which the groups were permuted.
#'
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel makeCluster detectCores stopCluster
#' @importFrom foreach foreach %do% %dopar%
#'
#' @examples "coming soon"
#'
#' @export
global_permutation_test <- function(data_,
                                    ...,
                                    group_col_name = "group_",
                                    id_col_name = "id_",
                                    event_col_name = "event_",
                                    ntrials = 10000,
                                    parallel = FALSE,
                                    ranseed = NaN,
                                    systematic = TRUE,
                                    na_fill = FALSE,
                                    check_ = FALSE,
                                    verbose = FALSE) {

  standard_data <- standard_table(data_,
                                  group_col_name = group_col_name,
                                  id_col_name = id_col_name,
                                  event_col_name = event_col_name,
                                  na_fill = na_fill)

  # warning("Note that the output of this function has changed. it is now a
  # list giving $p (the p-value), $stddev (the standard deviation) $error
  # (the Monte-Carlo error) and $N (the number of test statistics calculated).
  # Please see documentation.")

  teststat <- perm_test_statistic(
    standard_data$event_,
    standard_data$group_
  )

  num_trials <- min(ntrials, factorial(nrow(standard_data)))

  data_groups <- unique(standard_data$group_)

  permutation_test_type <- ifelse(
    length(data_groups) > 2,
    "Global", "Pairwise"
  )

  competing_groups <- ifelse(
    length(data_groups) > 2,
    "all groups", paste(data_groups, collapse=" vs. ")
  )

  if (parallel) {

    pool_size <- max(detectCores() - 2, 1)

    message(sprintf(
      "\nExecuting %s Permutation Test (%s) in parallel: %s workers",
      permutation_test_type, competing_groups, pool_size
    ))

    perm_cluster <- makeCluster(pool_size)
    registerDoSNOW(perm_cluster)

    prog_bar <- txtProgressBar(max = num_trials, style = 3)
    progress <- function(n) setTxtProgressBar(prog_bar, n)
    opts <- list(progress = progress)

    teststat_null <- tryCatch({

      foreach(1:num_trials,
              .combine = "c",
              .export=c("get_p_value"),
              .packages = c("dplyr"),
              .options.snow = opts) %dopar% {

        get_p_value(
          standard_data,
          ranseed = ranseed,
          systematic = systamatic,
          verbose = verbose
        )

      }
    },
    error = function(e) {
      message(sprintf("\nError encountered during parallel execution: %s", e))
      return(NaN)
    },
    warning = function(w) {
      message(sprintf(
        "\nWarning() encountered during parallel execution: %s",
        w
      ))
    }
    )

    message("\n")
    stopCluster(perm_cluster)

  } else {

    message(sprintf(
      "\n%s Permutation Test (%s): serial execution selected",
      permutation_test_type, competing_groups
    ))

    teststat_null <- array(data = NaN, dim = num_trials)

    prog_bar <- txtProgressBar(max = num_trials, initial = 0, style = 3)

    for (idx in 1:num_trials) {
      setTxtProgressBar(prog_bar, idx)
      teststat_null[idx] <- get_p_value(standard_data,
        ranseed = ranseed,
        systematic = systematic,
        verbose = verbose
      )
    }

    message("\n")
  }

  if (check_) {
    # check whether the members of teststat_null are different from each
    # other; can go wrong when we don;t use the perm_group_ column when
    # calculating the test statistic. make an upper triangular matrix out of
    # the values and calculate the determinant. test whether the determinant
    # is consistently e-close to the nth power of randomly chosen element of
    # the vector. if so, the elements on the vector are all the same
  }

  p_value <- mean(teststat_null >= teststat, na.rm = TRUE)
  mc_error <- sqrt(p_value*(1-p_value)/num_trials)
  num_stats <- length(teststat_null)
  num_obs <- nrow(standard_data)

  if (verbose) {

    message_text <- paste0(
      sprintf("\n%s Permutation Test:", permutation_test_type),
      sprintf("\n\nTotal number of obervations : %s.", num_obs),
      sprintf("\n\nGroups (N=%s): [%s].",
        length(data_groups),
        paste(data_groups, collapse = ", ")
      ),
      "\n\n",
      standard_data %>%
        group_by(group_) %>%
        summarise(N = n()) %>%
        mutate(text_ = sprintf(
          "\tGroup %s (%s): %s observations,",
          1:n(), group_, N
        )) %>%
        pull(text_) %>%
        paste0(collapse = "\n"),
      ".",
      sprintf(
        paste0(
          "\n\nTest Results:",
          "\n\tNumber of test statistics calculated: %s.",
          "\n\tp-value = %s, Monte-Carlo error = %s."
        ),
        num_stats, p_value, round(mc_error, 6)
      ),
      "\n\n***************"
    )

    message(message_text)
  }

  return(list(
    p = p_value,
    error = mc_error,
    N_trials = num_stats,
    N_obs = num_obs
  ))

}