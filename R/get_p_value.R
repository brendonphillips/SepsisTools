#' Permutation test helper
#'
#' standardises the input data table and retrieves the test statistic (p value)
#' 
#' @param data_ data frame with at least three columns: group, id and event
#' @param group_name name of the `group` column
#' @param id_name name of the `id` column
#' @param systematic whether the groups should be permuted consistently with 
#' the id, or not
#'
#' @returns The test statistic after even groups have been permuted
#'
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select mutate right_join tibble join_by all_of .data
#'
#' @example examples/get_p_value_example.R
#' 
#' @export
get_p_value <- function(data_, 
                         ..., 
                         group_name = "group_", 
                         id_name = "id_", 
                         event_name = "event_", 
                         systematic = FALSE,
                         na_fill = TRUE) {
    
    standardised_table <- data_ %>%
        select(all_of(c(id_name, group_name, event_name))) %>%
        mutate(.sep.entry.id = 1:n()) %>%
        rename(
            id_ = !!id_name,
            group_ = !!group_name,
            event_ = !!event_name
        ) %>%
        mutate(
            group_ = case_when(
                (na_fill) & is.na(group_) ~ ".NA_group",
                TRUE ~ group_
            )
        ) %>%
        subset(!is.na(group_))
    
    permuted_table <- permute_groups(standardised_table, systematic)
    
    teststat_perm <- perm_test_statistic(
        permuted_table$event_, 
        permuted_table$perm_group_
    )
    
    return(teststat_perm)
}
