#' Global Permutation Test
#'
#' Carry through the permutation test
#'
#' @param data_ data frame with at least 3 columns: event, id, group
#' @param group_col_name the name of the (IP ) column
#' @param id_col_name name of the (participant) id column
#' @param event_col_name name of the event column
#' @param ntrials the number of permutation tests in the ensemble
#' @param parallel True/False whether the permutations shosuld be run in
#' parallel for speed
#' @param ranseed fixed random seed, for reproducibility
#' @param systematic True/False whether the groups are to be grouped (similar
#' to the original entries) or totally random
#' @param na_fill if any entries in the input table have empty groups, fill them with a generated group name, or filter those rows out
#'
#' @return A list giving `$p` (the p-value) and `$error` (the Monte-Carlo error) of the calculation, `$N` the number of test statistics calculated
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
                                    group_col_name = "group",
                                    id_col_name = "id",
                                    event_col_name = "event",
                                    ntrials = 10000,
                                    parallel = FALSE,
                                    ranseed = NaN,
                                    systematic = TRUE, 
                                    na_fill = FALSE,
                                    check_ = FALSE) {
    
    # warning("Note that the output of this function has changed. it is now a list giving $p (the p-value), $stddev (the standard deviation) $error (the Monte-Carlo error) and $N (the number of test statistics calculated). Please see documentation.")
    
    if (!is.na(ranseed) && is.numeric(ranseed)) {
        set.seed(ranseed)
    } else {
        set.seed(Sys.time())
    }
    
    teststat <- perm_test_statistic(
        data_[[event_col_name]],
        data_[[group_col_name]]
    )
    
    if (parallel) {
        
        pool_size <- max(detectCores() - 2, 1)
        
        message(sprintf(
            "\nExecuting global permutation tests in parallel: %s workers",
            pool_size
        ))
        
        # perm_cluster <- makeCluster(max(detectCores() - 2, 1), type = "PSOCK")
        # registerDoParallel(perm_cluster)
        
        perm_cluster <- makeCluster(pool_size)
        registerDoSNOW(perm_cluster)
        
        prog_bar <- txtProgressBar(max = ntrials, style = 3)
        progress <- function(n) setTxtProgressBar(prog_bar, n)
        opts <- list(progress = progress)
        
        
        teststat_null <- tryCatch({
            
            foreach(1:min(ntrials, factorial(nrow(data_))),
                    .combine = "c",
                    .export=c("get_p_value"),
                    .packages = c("dplyr"),
                    .options.snow = opts) %dopar% {
                        
                        get_p_value(data_,
                                    group_col_name = group_col_name,
                                    id_col_name = id_col_name,
                                    event_col_name = event_col_name,
                                    systematic = systamatic,
                                    na_fill = na_fill)
                        
                        
                    }
        },
        error = function(e) {
            message(sprintf("\nError encountered during parallel execution: %s", e))
            return(NaN)
        },
        warning = function(w) {
            message(sprintf("\nWarning() encountered during parallel execution: %s", w))
        }
        )
        
        message("\n")
        stopCluster(perm_cluster)
        
    } else {
        
        teststat_null <- array(data = NaN, dim = ntrials)
        
        for (it in 1:ntrials) {
            teststat_null[it] <- get_p_value(data_,
                                           group_col_name = group_col_name,
                                           id_col_name = id_col_name,
                                           event_col_name = event_col_name,
                                           ranseed = ranseed,
                                           systematic = systematic,
                                           na_fill = na_fill)
        }
    }
    
    if (check_) {
        # check whether the members of teststat_null are different from each other; can go wrong when we don;t use the perm_group_ column when calculating the test statistic. make an upper triangular matrix out of the values and calculate the determinant. test whether the determinant is consistently e-close to the nth power of randomly chosen element of the vector. if so, the elements on the vector are all the same 
    }
    
    p_value <- mean(teststat_null >= teststat, na.rm = TRUE)
    mc_error <- sqrt(p_value*(1-p_value)/ntrials)
    num_res <- length(teststat_null)
    
    return(list(
        p = p_value, 
        error = mc_error, 
        N = num_res
    ))
    
}