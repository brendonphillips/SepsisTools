options(dplyr.summarise.inform = FALSE)

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' Test statistic calculation
#'
#' Calculate the test statistic (sum of square differences) between group means
#' and the overall mean
#' 
#' @param events The scores to be parsed
#' @param groups The groups that the scores belong to
#' 
#' @returns The sum of square differences between group means and the overall
#' event mean
#' 
#' @example examples/perm_test_statistic_example.R
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble mutate group_by summarise ungroup pull n
#'
#' @export
perm_test_statistic <- function(events, groups) {
  loc_events <- events
  loc_groups <- groups

  if (length(loc_groups) != length(loc_events)) {
    loc_groups <- rep(loc_groups, length.out = length(loc_events))
  }

  test_stat <- tibble(event = events, group = groups) %>%
    mutate(all_mean = sum(event) / nrow(.)) %>%
    group_by(group) %>%
    summarise(
        gp_mean = sum(event) / n(),
        all_mean = unique(all_mean)
    ) %>%
    ungroup() %>%
    summarise(hehe = sum((gp_mean - all_mean)**2)) %>%
    pull()

  # overall_rate <- mean(loc_events)
  # gp_rates <- tapply(loc_events, loc_groups, mean)
  # test_stat <- sum((gp_rates - overall_rate)^2)

  return(test_stat)
}


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
    
    if(! any(is.na( as.numeric(the_list) )) ) {
        
        if(is.na(zero_ref)){ 
            return(as.numeric(the_list))
        } else if(! as.numeric(zero_ref) %in% the_list) {
            warning(sprintf(warning_string, zero_ref, the_list[1]))
            return( as.numeric(the_list == the_list[1]) )
        } else {
            return( as.numeric(the_list == zero_ref) )
        }
    }
    
    # # if any element of the list is not castable to a number, create map 
    # # ourselves
    # if( any(is.na( as.numeric(the_list) )) ) {
    #     
    #     the_list <- as.character(list_)
    #     zero_elem <- the_list[1]
    #     
    #     if(is.na(zero_reference)) {
    #         
    #        
    #         
    #     } else if(! toString(zero_reference) %in% the_list) {
    #         warning(sprintf(
    #             "The given zero-reference member %s is not a member of the 
    #             given list. Using the first element of the list %s as the zero",
    #             zero_reference, the_list[1]
    #         ))
    #         zero_elem <- the_list[1]
    #     }
    #     
    #     return( as.numeric(the_list == zero_elem) )
    #     
    # } else {
    #     
    #     return(as.numeric(the_list))
    # }
}


#' Permutation test helper
#'
#' helper function for permutation test
#' 
#' @param data_ data frame with at least three columns: group, id and event
#' @param group_name name of the `group` column
#' @param id_name name of the `id` column
#' @param systematic whether the groups should be permuted consistently with the id, or not
#' 
#' @returns The test statistic after even groups have been permuted
#' 
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select mutate right_join tibble join_by
#' 
#' @examples "coming soon"
#' 
permgp_fn <- function(data_, group_name, id_name, event_name, systematic) {
    
  if (systematic == TRUE) {
      
    data_perm <- data_ %>%
    tibble %>%
      select(all_of(c(id_name, group_name))) %>%
      unique() %>%
      mutate(
        group_perm = sample(
          x = .data[[group_name]],
          size = nrow(.),
          replace = FALSE)
      ) %>%
      right_join(
        data_ %>% select(-all_of(c(group_name))),
        by = join_by(!!as.name(id_name))
      )

  } else {

    data_perm <- data_ %>%
      mutate(group_perm = sample(
        x = .data[[group_name]],
        size = nrow(.),
        replace = FALSE)
      )
  }

  teststat_perm <- perm_test_statistic(
    data_perm[[event_name]],
    data_perm$group_perm
  )

  return(teststat_perm)
}


#' Global Permutation Test
#'
#' Carry through the permutation test
#'
#' @param data_ data frame with at least 3 columns: event, id, group
#' @param group_name the name of the (IP ) column     
#' @param id_name name of the (participant) id column
#' @param event_name name of the event column
#' @param ntrials the number of permutation tests in the ensemble
#' @param parallel True/False whether the permutations shosuld be run in parallel for speed
#' @param ranseed fixed random seed, for reproducibility
#' @param systematic True/False whether the groups are to be grouped (similar to the original entries) or totally random
#' 
#' @return The p value of the test statistic (sum-of-square-differences)
#' 
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster detectCores stopCluster
#' @importFrom foreach foreach %do% %dopar%
#' 
#' @examples "coming soon"
#' 
#' @export
global_permutation_test <- function(data_, group_name = "group", id_name = "id", 
                             event_name = "event", ntrials = 1000, 
                             parallel = FALSE, ranseed = NaN, systematic = TRUE, 
                             ...) {
    
    if (!is.na(ranseed) & is.numeric(ranseed)) {
        set.seed(ranseed)
    } else {
        set.seed(Sys.time())
    }
    
    teststat <- perm_test_statistic(
        data_[[event_name]], 
        data_[[group_name]]
    )
    
    if (parallel) {
        
        permCluster <- makeCluster(max(detectCores()-2, 1), type = "PSOCK")
        registerDoParallel(permCluster)
        
        teststat_null <- tryCatch(
            {
                
                foreach(
                    1:ntrials,
                    .combine = "c",
                    .packages = c("dplyr"),
                    .export=c("permgp_fn", "perm_test_statistic")
                ) %dopar% {
                    
                    permgp_fn(data_, group_name, id_name, event_name, systematic)
                }
            },
            
            error = function(e) {
                print(sprintf("Error encountered during parallel execution: %s", e))
                return(NaN)
            },
            
            warning = function(w) { print("warnings encountered") }
        )
        
        stopCluster(permCluster)
        
    } else {
        
        teststat_null <- array(data = NaN, dim=ntrials)
        
        for (it in 1:ntrials) {
            teststat_null[it] <- permgp_fn(data_, group_name, id_name, event_name, systematic)
        }
    }
    
    p_value <- mean(teststat_null >= teststat, na.rm=TRUE)
    
    return(p_value)
    
}


#' Pairwise permutation tests
#' 
#' Conduct all possible permutation tests
#' 
#' @param first lorem
#' @param second ipsum
#' 
#' @return returning p values from pairwise tests
#' 
#' @importFrom dplyr tibble
#' 
#' @examples "coming soon"
#'
#' @export
pairwise_permutation_tests <- function(first, second, third) {
    return(NaN)
}


#' Step-down test (Holm procedure)
#' 
#' Function to carry through the step-down procedure after the permutation test
#' 
#' @param first lorem
#' @param second ipsum
#'
#' @return p.values and etc.
#' 
#' @importFrom dplyr tibble
#'
#' @examples "coming soon"
#'  
#' @export
step_down_test <- function(first, second) {
    return(NaN)
}