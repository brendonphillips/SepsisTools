#' Standardise the table
#' @export
standard_table <- function(data_,
                           ..., 
                           id_vector = c(),
                           group_vector = c(),
                           event_vector = c()) {
    if ()
}
    
    
    
    
    
    
    class_performance %>%
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
