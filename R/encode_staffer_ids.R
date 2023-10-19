#' String encoder
#'
#' encodes any string into a standardised RTxxxxx format
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble rowwise mutate left_join all_of rename_with
#' @importFrom rlang hash
#' @importFrom lazyeval lazy_dots
#'
#' @param .data (optional) the vector/list/data frame with ids
#' @param id_col the name of the column with IDs to be encoded (ignored
#'  if .data is a list)
#' @param out_col_name the name of the column with the encoded IDs (if NA,
#' the source column is overwritten and a column with a default name will
#' store the original information)
#' @param seed random seed to use (for reproducibility of results),
#' @param na_fill treat NA as a group and give them an ID
#' @param ... currently ignored
#'
#' @return a data frame / list with id_col replaced with new Sepsis IDs, with
#' pre-encoding column appended to the end with Sys.time suffix
#'
#' @example examples/encode_staffer_ids_example.R
#'
#' @export
encode_staffer_ids <- function(data_,
                               ...,
                               id_col = NA,
                               out_col_name = NA,
                               seed = NaN,
                               na_fill = FALSE) {

  encode_id_help <- function(the_id) {

    hash_split <- strsplit(hash(the_id), split = "")[[1]]

    hash_digit_posi <- grep("\\d", hash_split)
    hash_digit_list <- as.numeric(hash_split[hash_digit_posi])

    final_id <- (hash_digit_posi * hash_digit_list) %>%
      sample %>%
      .[c(TRUE, FALSE)] %>%
      as.character() %>%
      {paste0(c("RL", .), collapse = "")} %>%
      substr(1, 7)

    return(final_id)
  }

  if (!is.na(seed)) {
    old_seed <- .Random.seed
    on.exit({.Random.seed <<- old_seed})
    set.seed(seed)
  }

  # TODO: if table give with no input and output names, put error code

  current_id_col <- gsub('\\"', "", deparse(substitute(id_col)))
  out_stub <- deparse(substitute(out_col_name))

  if (is.na(out_stub) | out_stub %in% c("NA", "NAN")) {
    # the overwrite case
    rand_suffix <- round(as.numeric(Sys.time()))
    new_id_col <- current_id_col
    old_id_col <- sprintf("%s%s", current_id_col, rand_suffix)
  } else {
    new_id_col <- gsub('\\"', "", out_stub)
    old_id_col <- current_id_col
  }

  if (is.data.frame(data_)) {

    id_vec <- tryCatch({
      data_[[current_id_col]]
    },
    error = function(e) {
      message(sprintf("Caught warning while getting ID column: '%s'.\n
                      Input table unchanged.", e))
      return(data_)
    },
    warning = function(w) {
      message(sprintf("Warning encountered while getting ID column: '%s'", w))
    })

  } else {
    id_vec <- data_
  }

  if (length(id_vec) == 0 || all(is.na(id_vec))) {
    message("no IDs found. returning the input object")
    return(data_)
  }

  dict <- unique(id_vec) %>%
    .[!is.na(.) | na_fill] %>%
    {tibble(!!current_id_col := .)} %>%
    rowwise() %>%
    mutate(
      join_col = get(current_id_col),
      !!old_id_col := get(current_id_col),
      !!new_id_col := encode_id_help(get(current_id_col))
    )

  if (is.data.frame(data_)) {
    final_obj <- data_ %>%
      left_join(
        dict,
        by = setNames("join_col", current_id_col),
        keep = FALSE
      ) %>%
      select(-all_of(c(current_id_col))) %>%
      rename_with(~ gsub(".y$", "", .x))

  } else {
    final_obj <- dict[[new_id_col]]
  }

  return(final_obj)
}