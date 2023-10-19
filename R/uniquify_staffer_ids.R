data("staffer_dictionary")

#' Uniquify Staffer IDs
#'
#' A lookup junction to get unique IDs for each of the icddr,b staffers
#' (field, lab, etc) that worked on the SEPSIS project and recorded
#' information for any of the encounters
#'
#' @param data_ data frame or list with the raw IDs to be uniquified
#' @param ID_col the name of the column with the IDs to be uniquified (if
#' `data_` is a table, ignored otherwise)
#' #' @param ID_col the name of the column with IDs to be encoded (ignored
#' if .data is a list)
#' @param out_col_name the name of the column with the encoded IDs (if NA,
#' the source column is overwritten and a column with a default name will
#' store the original information)
#' @param ... currently ignored
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate left_join rename
#' @importFrom lazyeval lazy_dots
#'
#' @return a list/data frame with the unique RothLab ID to allow tracing of
#' activity throughout the encounters
#'
#' @example examples/uniquify_staffer_ids_example.R
#'
#' @export
uniquify_staffer_ids <- function(data_, ..., ID_col = NA, out_col_name = NA) {

  current_id_col <- gsub('\\"', "", deparse(substitute(ID_col)))
  out_name <- gsub('\\"', "", deparse(substitute(out_col_name)))

  rand_suffix <- round(as.numeric(Sys.time()))

  rand_chrw_name <- sprintf("chrw_id%s", rand_suffix)
  rand_encoded_name <- sprintf("encoding%s", rand_suffix)
  rand_curr_name <- sprintf("%s%s", current_id_col, rand_suffix)

  if (is.data.frame(data_)) {

    dict_joined <- staffer_dictionary %>%
      rename(
        !!rand_chrw_name := chrw_id,
        !!rand_encoded_name := encoding
      ) %>%
      right_join(
        data_,
        by = setNames(current_id_col, rand_chrw_name)
      )

    if (is.na(out_name) | out_name %in% c("NA", "NAN")) {

      # the overwrite case
      final_obj <- dict_joined %>%
        rename(
          !!current_id_col := !!rand_encoded_name,
          !!rand_curr_name := !!rand_chrw_name
        )

    } else {

      # the overwrite case
      final_obj <- dict_joined %>%
        rename(
          !!out_name := !!rand_encoded_name,
          !!current_id_col := !!rand_chrw_name
        )
    }

  } else {

    final_obj <- staffer_dictionary %>%
      subset(chrw_id %in% unlist(data_)) %>%
      pull(encoding)
  }

  return(final_obj)

}