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
                           na_fill = TRUE) {

  if (is.data.frame(data_)) {

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

