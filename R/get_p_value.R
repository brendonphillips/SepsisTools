# # fucking complicated logic oing on over here
# # function will standardise the names of the
# 
# # must add an id number if none is given
# global_permutation_test <- function() {
#     
#     function(data_, group_name, id_name, event_name, systematic) {
# teststat_perm <- perm_test_statistic(
#     data_perm[[event_name]],
#     data_perm$group_perm
# )
#     return(teststat_perm)
# }

get_p_value_ <- function(data_, 
                         ..., 
                         group_name = "group_", 
                         id_name = "id_", 
                         event_name = "event_", 
                         systematic = FALSE,
                         na_fill = TRUE) {
    
    standardised_tab_ <- data_ %>%
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
        )
    
    permed_table_ <- permute_groups(standardsed_table_, systematic)
    
    differneces <- 
        
        teststat_perm <-  perm_test_statistic(
            data_perm[[event_name]],
            data_perm$group_perm
        )
    return(teststat_perm)
}

}