#' Converting events vector to numerical (incomplete)
#'
#' helper function allowing the permutation test to work on input event vectors 
#' of various types (string, numeric, boolean) without preprocessing by the user
#'
#' @param list_ list of events of type numeric, bool, string
#' @param zero_reference the event to be assigned 0; other events will have the
#' value 1; if list_ is numeric and no `zero_reference` is given, the numerical
#' values in the list will be used
#' @param .. currently ignored
#'
#' @returns A numerical vector; either 0/1 or -Inf:Inf, depending on parameters
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr right_join
#'
#' @example examples/events_to_num_example.R
#'
#' @export
events_to_num <- function(list_, zero_reference = NaN, ...) {
    zero_ref <- zero_reference
    the_list <- list_
    
    warning_string <-  gsub(
        " +", " ",
        "The given zero-reference member `%s` is not a member of the 
    given list. Using the first element of the list `%s` as the zero"
    )
    
    # fix this function for booleans
    
    if (! any(is.na(as.numeric(the_list)))) {
        
        if (is.na(zero_ref)) {
            return(as.numeric(the_list))
            
        } else if(! as.numeric(zero_ref) %in% the_list) {
            
            warning(sprintf(warning_string, zero_ref, the_list[1]))
            return(as.numeric(the_list == the_list[1]))
            
        } else {
            return(as.numeric(the_list == zero_ref))
        }
    }
    
    # # if any element of the list is not castable to a number, create map 
    # # ourselves
    # if( any(is.na( as.numeric(the_list) )) ) {
    
    # the_list <- as.character(list_)
    # zero_elem <- the_list[1]
    
    # if(is.na(zero_reference)) {
    
    
    
    # } else if(! toString(zero_reference) %in% the_list) {
    # warning(sprintf(
    # "The given zero-reference member %s is not a member of the 
    # given list. Using the first element of the list %s as the zero",
    # zero_reference, the_list[1]
    # ))
    # zero_elem <- the_list[1]
    # }
    
    # return( as.numeric(the_list == zero_elem) )
    
    # } else {
    
    # return(as.numeric(the_list))
    # }
}
