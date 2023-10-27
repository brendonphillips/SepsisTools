#' Global Permutation Test
#'
#' Carry through the permutation test
#'
#' @param data_ the data set, with at least three features: event (what 
#' happened), id (who were they), group (what intervention were they 
#' administered)
#' @param ... currently ignored
#' @param group_col_name the name of the column with `group` information 
#' @param id_col_name the name of the column with participant/patient `id` 
#' information
#' @param event_col_name the name of the column with the event (data type 
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
#' @param verbose TRUE/FALSE whether a completion message with stats should be
#' printed to the screen whenever the permutation test ends. A progress bar is
#' printed in all cases.
#'
#' @return A list giving `$p` (the p-value), `$error` (the Monte-Carlo
#' error) of the calculation, `$N_trials` the number of permutations performed/
#' test statistics calculated, `$N_obs` the total number of observations for 
#' which the groups were permuted.
#'
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel makeCluster detectCores stopCluster
#' @importFrom foreach foreach %do% %dopar%
#'
#' @example examples/global_permutation_test_example.R
#'
#' @export
global_permutation_test <- function(data_,
                                    # ...,
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


  if (!is.na(ranseed)) {
    old_seed <- .Random.seed
    on.exit({.Random.seed <<- old_seed})
    set.seed(ranseed)
  }

  seed_vector <- sample(1:1e9, size = num_trials, replace = FALSE)
  
  teststat_null <- array(data = NaN, dim = num_trials)

  if (parallel) {

    pool_size <- max(detectCores() - 2, 1)

    message(gsub("\n$", "", sprintf(
      "\nExecuting %s Permutation Test (%s) in parallel: %s workers.\n%s",
      permutation_test_type, competing_groups, pool_size,
      ifelse(is.na(ranseed), "Warning: ***NO SEED WAS SET FOR THE RANDOM NUMBER GENERATOR***.", "")
    )))

    perm_cluster <- makeCluster(pool_size)
    registerDoSNOW(perm_cluster)

    prog_bar <- txtProgressBar(max = num_trials, style = 3)
    progress <- function(n) setTxtProgressBar(prog_bar, n)
    opts <- list(progress = progress)

    teststat_null <- tryCatch({

      foreach(idx = 1:num_trials,
              .combine = "c",
              .export=c("single_permutation"),
              .packages = c("dplyr"),
              .options.snow = opts) %dopar% {

        single_permutation(
          standard_data,
          ranseed = seed_vector[idx],
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

    message(gsub("\n$", "", sprintf(
      trimws("\n%s Permutation Test (%s): serial execution selected.\n%s"),
      permutation_test_type, competing_groups, 
      ifelse(is.na(ranseed), "Warning: ***NO SEED WAS SET FOR THE RANDOM NUMBER GENERATOR***.", "")
    )))

    prog_bar <- txtProgressBar(max = num_trials, initial = 0, style = 3)

    for (idx in 1:num_trials) {
      setTxtProgressBar(prog_bar, idx)
      teststat_null[idx] <- single_permutation(standard_data,
        ranseed = seed_vector[idx],
        systematic = systematic,
        verbose = verbose
      )
    }

    message("\n")
  }

  # ret_obj <- teststat_null
    
  # if (check_) {
  #   # check whether the members of teststat_null are different from each
  #   # other; can go wrong when we don;t use the perm_group_ column when
  #   # calculating the test statistic. make an upper triangular matrix out of
  #   # the values and calculate the determinant. test whether the determinant
  #   # is consistently e-close to the nth power of randomly chosen element of
  #   # the vector. if so, the elements on the vector are all the same
  # }

  p_value <- mean(teststat_null > teststat, na.rm = TRUE)
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

  ret_obj <- list(
    # test_stat = teststat,
    # test_stat_null = teststat_null,
    p = p_value,
    error = mc_error,
    N_trials = num_stats,
    N_obs = num_obs
  )
  
  return(ret_obj)

}