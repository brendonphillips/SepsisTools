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
                                       ntrials = 100,
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
        
        data_groups <- unique(data_[[group_name]])
        
        message_text <- paste0(
            "\nPreliminary Global Test:", 
            sprintf("\n\nNumber of obervations : %s.", nrow(data_)),
            sprintf("\n\nGroups (N=%s): [%s].", 
                    length(data_groups),
                    paste(data_groups, collapse=", ")
            ),
            "\n\n",
            data_ %>% 
                rename(group_ := group_name) %>%
                group_by(group_) %>% 
                summarise(N = n()) %>%
                mutate(text_ = sprintf(
                    "\tGroup %s (%s): %s observations,", 
                    1:n(), group_, N)
                ) %>% 
                pull(text_) %>% 
                paste0(collapse="\n"),
            ".",
            sprintf(
                paste0(
                    "\n\nGlobal Test:",
                    "\n\n\tNumber of test statistics calculated: %s.",
                    "\n\tp-value = %s, Monte-Carlo error = %s."
                ),
                gt_results$N, gt_results$p, round(gt_results$error, 6)
            ),
            "\n\n"
        )
        
        message(message_text)
        
        if (! reference_group %in% data_groups) {
            stop(sprintf(
                "Requested reference group `%s` is not among the groups in the %s data frame. Aborting pairwise permutation test.", 
                reference_group, deparse(substitute(data_))
            ))
        }
        
        for(other_group in )
        
    }

    return(0)
    
}