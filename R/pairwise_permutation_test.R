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
                                       group_name = "group",
                                       id_name = "id",
                                       event_name = "event",
                                       ntrials = 3, # 10000,
                                       parallel = FALSE,
                                       ranseed = NaN,
                                       systematic = TRUE,
                                       na_fill = FALSE,
                                       backsolve = FALSE,
                                       reference_group = "placebo",
                                       step_down_procedure = "HB",
                                       global_test_first = TRUE) {
    
    if (global_test_first) {
        
        gt_results <- global_permutation_test(
            data_,
            group_name = group_name,
            id_name = id_name,
            event_name = event_name,
            ntrials = ntrials,
            parallel = parallel,
            ranseed = ranseed,
            systematic = systematic,
            na_fill = na_fill
        )
        
        message_text <- sprintf(paste0("Results of Global Permutation Test:\n", 
                                "N obervations : %s, N groups: %s, N test statistics calculated: %s\n",
                                "Distinct groups: (%s)\n",
                                "p-value = %s, Monte-Carlo error = %s"),
                                nrow(data_), length(unique(class_performance[["class_teacher"]])), gt_results$N,
                                paste(unique(class_performance[["class_teacher"]]), collapse=", "),
                                gt_results$p, round(gt_results$error, 4)
                                )
        
        message(message_text)
        
    }

    return(0)
    
}