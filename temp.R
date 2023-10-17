#' is_systematic function
#' 
#' 
#' @param DT data table to determine systemicity
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' 
is_systematic <- function(DT) {
    return(
        DT %>%
            group_by(part_id) %>%
            summarise(split_acros_n_groups = length(unique(group_perm))) %>%
            subset(split_acros_n_groups > 1) %>%
            {nrow(.) == 0}
    )
}




#' 
#' # events <- c(1,0,0,1,1)
#' # groups <- c(1,2,1,2,1)
#' # 
#' # system.time(haha <- tibble(event = events, group = groups) %>%
#' #   mutate(all_mean = sum(event) / nrow(.)) %>%
#' #   group_by(group) %>%
#' #   summarise(
#' #     gp_mean = sum(event) / n(),
#' #     all_mean = unique(all_mean)
#' #   ) %>%
#' #   ungroup() %>%
#' #   summarise(hehe = sum((gp_mean - all_mean)**2)) %>%
#' #   pull())
#' # 
#' # system.time(perm_test_statistic(events, groups))
#' # 
#' # tapply(events, groups, mean)
#' # mean(events)
#' 
#' # library(rstudioapi)
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# 
# library(readr)
# 
# DF <- read_csv("data.csv") %>%
#     select(-any_of(c("...1", "X"))) %>%
#     mutate(across(c(everything(), -event), factor)) %>%
#     mutate()
# 
# group_name = "group"
# id_name = "part_id"
# event_name = "event"
# 
# test_p <- perm_test(
#         DF, 
#         groupname = group_name,
#         idname = id_name,
#         eventname = event_name,
#         parallel = FALSE, 
#         ntrials=1000, 
#         ran_seed = 301031,
#         systematic = TRUE
#     )
# 
# print(test_p)



