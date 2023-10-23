#' Pairwise permutation tests
#'
#' Conduct all possible permutation tests
#'
#' @param first lorem
#' @param second ipsum
#'
#' @return returning p values from pairwise tests
#'
#' @importFrom dplyr tibble
#' @importFrom stats p.adjust
#'
#' @examples "coming soon"
#'
#' @export
pairwise_permutation_tests <- function(data_,
                                       ...,
                                       group_col_name = "group",
                                       id_col_name = "id",
                                       event_col_name = "event",
                                       ntrials = 100,
                                       parallel = FALSE,
                                       ranseed = NaN,
                                       systematic = TRUE,
                                       na_fill = FALSE,
                                       backsolve = FALSE,
                                       reference_group = "placebo",
                                       step_down_procedure = "HB",
                                       global_test_first = TRUE,
                                       verbose = FALSE) {
    
    if (global_test_first) {
        
        # if global test is done first, always send the message to the user
        gt_results <- global_permutation_test(
            data_,
            group_col_name = group_col_name,
            id_col_name = id_col_name,
            event_col_name = event_col_name,
            ntrials = ntrials,
            parallel = parallel,
            ranseed = ranseed,
            systematic = systematic,
            na_fill = na_fill,
            verbose = TRUE
        )
    }
    
    data_groups <- unique(data_[[group_col_name]])
    
    if (! reference_group %in% data_groups) {
        stop(sprintf(
            "Requested reference group `%s` is not among the groups in the %s data frame. Aborting pairwise permutation test.", 
            reference_group, deparse(substitute(data_))
        ))
    }
    
    pairwise_test_results <- list()
    
    for(other_group in data_groups[data_groups != reference_group]) {
        
        next   
    }
    
    return(0)
    
}