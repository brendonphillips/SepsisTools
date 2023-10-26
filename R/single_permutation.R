#' Permutation test helper
#'
#' standardises the input data table and retrieves the test statistic (p value)
#'
#' @param data_ the data set, with at least three features: event (what 
#' happened), id (who were they), group (what intervention were they 
#' administered)
#' @param ... currently ignored
#' @param group_col_name the name of the column with `group` information 
#' @param id_col_name the name of the column with participant/patient `id` 
#' information
#' @param event_col_name the name of the column with the event (data type 
#' castable to numerical)
#' @param systematic True/False whether the groups are to be grouped or 
#' distributed totally randomly. For example, should all observations from 
#' Patient A in group (G1) be permuted to the same group (G2), or can they be 
#' spread across permuted groups? The first option (TRUE) is endorsed by 
#' Eleanor, the second option (FALSE) would be in line with other R packages 
#' (`coin`, for example).
#' @param ranseed a set random seed to be used for the permutations (used for 
#' reproducibility)
#'#' @param na_fill if any entries in the input table have empty groups, either 
#'(TRUE) fill them with a generated group name .NA_group, or (FALSE) filter 
#'those rows out.
#'
#' @returns The test statistic after even groups have been permuted
#'
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select mutate right_join tibble join_by all_of .data n
#'
#' @export
single_permutation <- function(data_,
                        ...,
                        group_col_name = "group_",
                        id_col_name = "id_",
                        event_col_name = "event_",
                        systematic = FALSE,
                        na_fill = TRUE,
                        ranseed = NA) {

  # same for standard_data - need to figure out how to get these functions to
  # link correctly when running in parallel

  # standard_data <- standard_table(data_,
  #                                 group_col_name = group_col_name,
  #                                 id_col_name = id_col_name,
  #                                 event_col_name = event_col_name,
  #                                 na_fill = na_fill)

  ##### must get rid of
  standard_data <- data_ %>%
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


  if (!is.na(ranseed)) {
    old_seed <- .Random.seed
    on.exit({.Random.seed <<- old_seed})
    set.seed(ranseed)
  }

  #####

  # permuted_table <- permute_groups(standardised_table, systematic)

  # TODO: a work-around for the parallel processing, I've copied the body
  # of the permute_groups function into single_permutation. we need to investigate
  # why I had endless problems with permuted_table, but perm_test_statistic
  # is not a problem, and can be loaded conventionally and without workarounds.
  # There's a fix there; let's get to it. TECHNICAL DEBT

  ###### must get rid of
  permuted_table <- standard_data %>%
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
      standard_data,
      by = join_by(id_, group_)
    )
  ######

  teststat_perm <- perm_test_statistic(
    permuted_table$event_,
    permuted_table$perm_group_
  )

  return(teststat_perm)
}
