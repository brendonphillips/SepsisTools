#' Permutation test helper
#'
#' standardises the input data table and retrieves the test statistic (p value)
#' 
#' @param data_ data frame with at least three columns: group, id and event
#' @param group_col_name name of the `group` column
#' @param id_col_name name of the `id` column
#' @param systematic whether the groups should be permuted consistently with 
#' the id, or not
#'
#' @returns The test statistic after even groups have been permuted
#'
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select mutate right_join tibble join_by all_of .data
#' 
#' @export
get_p_value <- function(data_, 
                         ..., 
                         group_col_name = "group_", 
                         id_col_name = "id_", 
                         event_col_name = "event_", 
                         systematic = FALSE,
                         na_fill = TRUE) {
    
# TODO: a work-around for the parallel processing, I've copied the body of the permute_groups function into get_p_value. we need to investigate why I had endless problems with permuted_table, but perm_test_statistic is not a problem, and can be loaded conventionally and without workarounds. There's a fix there; let's get to it. TECHNICAL DEBT
    
    standardised_table <- data_ %>%
        select(all_of(c(id_col_name, group_col_name, event_col_name))) %>%
        mutate(.sep.entry.id = 1:n()) %>%
        rename(
            id_ = !!id_col_name,
            group_ = !!group_col_name,
            event_ = !!event_col_name
        ) %>%
        mutate(
            group_ = case_when(
                (na_fill) & is.na(group_) ~ ".NA_group",
                TRUE ~ group_
            )
        ) %>%
        subset(!is.na(group_))
    
    # permuted_table <- permute_groups(standardised_table, systematic)
    
    ###### must get rid of
    permuted_table <- standardised_table %>%
        tibble %>%
        select(id_, group_) %>%
        unique() %>%
        mutate(
            perm_group_ = sample(
                x = group_,
                size = nrow(.),
                replace = FALSE)
        ) %>%
        right_join(
            standardised_table,
            by = join_by(id_, group_)
        )
    
    ######
    
    teststat_perm <- perm_test_statistic(
        permuted_table$event_, 
        permuted_table$perm_group_
    )
    
    return(teststat_perm)
}
