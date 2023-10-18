#' String encoder
#' 
#' encodes any string into a standardised SKxxxxx format
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble rowwise mutate left_join
#' @importFrom rlang hash
#' @importFrom lazyeval lazy_dots
#'
#' @param .data (optional) the vector/list/data frame with ids
#' @param ID_col the name of the column with IDs to be encoded (ignored if .data is a list)
#' @param out_col_name the name of the column with the encoded IDs (if NA, the source column is overwritten and a column with a default name will store the original information)
#' @param seed random seed to use (for reproducibility of results),
#' @param na.fill treat NA as a group and give them an ID
#' @param ... currently ignored
#' 
#' @return a data frame / list with ID_col replaced with new Sepsis IDs, with pre-encoding column appended to the end with Sys.time suffix
#' 
#' @example examples/fix_chrw_ids_example.R
#' 
#' @export
encode_IDs <- function(data_, ..., ID_col = NA, out_col_name = NA, seed = NaN, na.fill = FALSE) {
    
    encode_ID_help <- function(the_ID) {
        
        hash_split <- strsplit(hash(the_ID), split="")[[1]]
        
        hash_digit_posi <- grep("\\d", hash_split)
        hash_digit_list <- as.numeric(hash_split[hash_digit_posi])
        
        final_ID <- (hash_digit_posi * hash_digit_list) %>%
            sample %>%
            .[c(TRUE, FALSE)] %>%
            as.character() %>%
            {paste0(c("RL", .), collapse="")} %>%
            substr(1, 7)
        
        return(final_ID)
    }
    
    if (!is.na(seed)) {
        old.seed <- .Random.seed
        on.exit({.Random.seed <<- old.seed})
        set.seed(seed)
    }
    
    # TODO: if table give with no input and output names, put error code
    
    current_ID_col <- gsub('\\"', "", deparse(substitute(ID_col)))
    out_stub <- deparse(substitute(out_col_name))
    
    if (is.na(out_stub) | out_stub %in% c("NA", "NAN")) {
        # the overwrite case
        rand_suffix <- round(as.numeric(Sys.time()))
        new_ID_col <- current_ID_col
        old_ID_col <- sprintf("%s%s", current_ID_col, rand_suffix)
    } else {
        new_ID_col <- gsub('\\"', "", out_stub)
        old_ID_col <- current_ID_col
    }

    if (is.data.frame(data_)) {

        ID_vec <- tryCatch({
            data_[[current_ID_col]]
        },
        error = function(e) {
            message(sprintf("Caught warning while getting ID column: '%s'.\nInput table unchanged.", e))
            return(data_)
        },
        warning = function(w) {
            message(sprintf("Warning encountered while getting ID column: '%s'", w))
        })
        
    } else {
        
        ID_vec = data_
    }

    if (length(ID_vec) == 0 | all(is.na(ID_vec))) {
        message("no IDs found. returning the input object")
        return(data_)
    }

    dict <- unique(ID_vec) %>%
        .[!is.na(.) | na.fill] %>%
        { tibble(!!current_ID_col := .)} %>%
        rowwise %>%
        mutate(
            join_col = get(current_ID_col),
            !!old_ID_col := get(current_ID_col),
            !!new_ID_col := encode_ID_help(get(current_ID_col))
        )

    if (is.data.frame(data_)) {
        final_obj <- data_ %>%
            left_join(
                dict,
                by = setNames("join_col", current_ID_col),
                keep = FALSE
            )  %>%
            select(-all_of(c(current_ID_col))) %>%
            rename_with(~ gsub(".y$", "", .x))

            } else {
        final_obj <- dict[[new_ID_col]]
    }

    return(final_obj)
    
}

data("staffer_dictionary")

#' Uniquify Staffer IDs
#'
#' A lookup junction to get unique IDs for each of the icddr,b staffers (field, lab, etc) that worked on the SEPSIS project and recorded information for any of the encounters
#'
#' @param data_ data frame or list with the raw IDs to be uniquified
#' @param ID_col the name of the column with the IDs to be uniquified (if `data_` is a table, ignored otherwise)
#' #' @param ID_col the name of the column with IDs to be encoded (ignored if .data is a list)
#' @param out_col_name the name of the column with the encoded IDs (if NA, the source column is overwritten and a column with a default name will store the original information)
#' @param ... currently ignored
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate left_join
#' @importFrom lazyeval lazy_dots
#' 
#' @return a list/data frame with the unique RothLab ID to allow tracing of activity throughout the encounters
#' 
#' @examples "coming soon"
#' 
#' @export
uniquify_chrw <- function(data_, ..., ID_col = NA, out_col_name = NA) {
    
}