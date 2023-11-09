#' Standardise the table
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble select all_of mutate rename case_when
#'
#' @param data_ lorem
#' @param ... ipsum
#' @param id_Vector dolor
#' @param group_vector si
#' @param event_vector amet
#'
#' @return a standardised table to avoid faffing around with dynamic names
#' and such
#'
#' @export
standard_table <- function(data_,
                           ...,
                           id_col_name = "id_",
                           group_col_name = "group_",
                           event_col_name = "event_",
                           id_vector = c(),
                           group_vector = c(),
                           event_vector = c(),
                           na_fill = TRUE,
                           data_frame_name = "input") {

  if (is.data.frame(data_)) {
    
    names_data <- names(data_)
    
    if(! id_col_name %in% names_data) {
      stop(sprintf(
        "Requested id column `%s` is not a feature in the %s data frame. Options are: ['%s'].",
        id_col_name, data_frame_name, paste(names_data, collapse = "', '")
      ))
    }
    
    if(! group_col_name %in% names_data) {
      stop(sprintf(
        "Requested group column `%s` is not a feature in the %s data frame.. Options are: ['%s'].",
        group_col_name, data_frame_name, paste(names_data, collapse = "', '")
      ))
    }
    
    if(! event_col_name %in% names_data) {
      stop(sprintf(
        "Requested event column `%s` is not a feature in the %s data frame.. Options are: ['%s'].",
        event_col_name, data_frame_name, paste(names_data, collapse = "', '")
      ))
    }

    stan_tab <- data_ %>%
      tibble() %>%
      select(all_of(c(id_col_name, group_col_name, event_col_name))) %>%
      mutate(.sep.entry.id = 1:n()) %>%
      rename(
        id_ = !!id_col_name,
        group_ = !!group_col_name,
        event_ = !!event_col_name
      ) %>%
      mutate(
        id_ = as.character(id_),
        group_ = as.character(group_),
        group_ = case_when(
          (na_fill) & is.na(group_) ~ ".NA_group",
          TRUE ~ group_
        )
      ) %>%
      subset(!is.na(group_))

  } else {
    # technical debt for later
    stan_tab <- tibble()
  }

  return(stan_tab)
}

