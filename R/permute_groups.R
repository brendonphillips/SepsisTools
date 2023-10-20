#' Group permutation
#'
#' function to carry out the group permutations
#' 
#' @param data_ data frame with at least three columns: group, id and event. this is created by the global_permutation_test function call
#' @param systematic whether the groups should be permuted consistently with 
#' the id, or not
#'
#' @returns data frame (tibble) with both the original and permuted groups
#'
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select mutate right_join tibble join_by all_of .data
#'
#' @examples "coming soon"
#' ## standard col names, id_no, group_no, column_no
#' 
#' @export
permute_groups <- function(data_, systematic) {
    
    if (systematic == TRUE) {
        
        data_perm <- data_ %>%
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
                data_,
                by = join_by(id_, group_)
            )
        
    } else {
        
        data_perm <- data_ %>%
            mutate(
                perm_group_ = sample(
                    x = group_,
                    size = nrow(.),
                    replace = FALSE
                )
            )
    }
    
    return(data_perm)
}