#' Global Permutation Test
#'
#' Carry through the permutation test
#'
#' @param data_ data frame with at least 3 columns: event, id, group
#' @param group_name the name of the (IP ) column
#' @param id_name name of the (participant) id column
#' @param event_name name of the event column
#' @param ntrials the number of permutation tests in the ensemble
#' @param parallel True/False whether the permutations shosuld be run in
#' parallel for speed
#' @param ranseed fixed random seed, for reproducibility
#' @param systematic True/False whether the groups are to be grouped (similar
#' to the original entries) or totally random
#'
#' @return The p value of the test statistic (sum-of-square-differences)
#'
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster detectCores stopCluster
#' @importFrom foreach foreach %do% %dopar%
#'
#' @examples "coming soon"
#'
#' @export
global_permutation_test <- function(data_,
                                    ...,
                                    group_name = "group",
                                    id_name = "id",
                                    event_name = "event",
                                    ntrials = 10000,
                                    parallel = FALSE,
                                    ranseed = NaN,
                                    systematic = TRUE) {
    if (!is.na(ranseed) && is.numeric(ranseed)) {
        set.seed(ranseed)
    } else {
        set.seed(Sys.time())
    }
    
    teststat <- perm_test_statistic(
        data_[[event_name]],
        data_[[group_name]]
    )
    
    if (parallel) {
        
        perm_cluster <- makeCluster(max(detectCores() - 2, 1), type = "PSOCK")
        registerDoParallel(perm_cluster)
        
        teststat_null <- tryCatch({
            foreach(1:max(ntrials, factorial(nrow(data_))),
                    .combine = "c",
                    .packages = c("dplyr"),
                    .export=c("permgp_fn", "perm_test_statistic")) %dopar% {
                        permgp_fn(data_, group_name, id_name, event_name, systematic)
                    }
        },
        error = function(e) {
            print(sprintf("Error encountered during parallel execution: %s", e))
            return(NaN)
        },
        warning = function(w) {
            print("warnings encountered")
        }
        )
        
        stopCluster(perm_cluster)
        
    } else {
        
        teststat_null <- array(data = NaN, dim = ntrials)
        
        for (it in 1:ntrials) {
            teststat_null[it] <- permgp_fn(data_,
                                           group_name,
                                           id_name,
                                           event_name,
                                           systematic)
        }
    }
    
    p_value <- mean(teststat_null >= teststat, na.rm = TRUE)
    
    return(p_value)
    
}