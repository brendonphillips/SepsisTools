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
                                       step_down_procedures = c("BH"),
                                       global_test_first = TRUE,
                                       verbose = TRUE) {
    
    if (global_test_first) {
        
        # # if global test is done first, always send the message to the user
        # gt_results <- global_permutation_test(
        #     data_,
        #     group_col_name = group_col_name,
        #     id_col_name = id_col_name,
        #     event_col_name = event_col_name,
        #     ntrials = ntrials,
        #     parallel = parallel,
        #     ranseed = ranseed,
        #     systematic = systematic,
        #     na_fill = na_fill,
        #     verbose = TRUE
        # )
    }
    
    
    data_groups <- unique(data_[[group_col_name]])
    if (! reference_group %in% data_groups) {
        stop(sprintf(
            "Requested reference group `%s` is not among the groups in the %s data frame. Aborting pairwise permutation test.", 
            reference_group, deparse(substitute(data_))
        ))
    }
    other_groups <- data_groups %>% .[. != reference_group] %>% unique
    
    pairwise_test_results <- list()
    
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
        ) # %>%
        # relocate(sym(group_name), N, p, error)
    

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
            cbind(final_res)
    }

    return(final_res)

}

# of only 2 groups, change message to paireiser test
# if only a single group, thenthrow error
# column names in lower case to avoid error