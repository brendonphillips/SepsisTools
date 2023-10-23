rm(list = ls())

setwd("C:/Users/brendon phillips/Documents/GitHub/rothlab_permtest/")

library(devtools)
library(dplyr)

# detach("package:SepsisTools", unload = TRUE)
# devtools::install_github("brendonphillips/SepsisTools", ref="main", force=TRUE)

# devtools::document(); devtools::load_all()
# 
library(SepsisTools)

devtools::document()
devtools::load_all()

data("class_performance")

# perm <- global_permutation_test(
#     haha,
#     group_col_name = "group",
#     id_col_name = "part_id",
#     event_col_name = "event",
#     systematic = TRUE,
#     ntrials = 10000,
#     parallel = TRUE,
#     ranseed = 20000
# )
# 
# temp <- class_performance %>%
#     select(student_id, class_teacher, passed_exam_num) %>%
#     rename(
#         id_ = student_id,
#         group_ = class_teacher,
#         event_ = passed_exam_num
#     ) %>%
#     mutate(
#         id_ = as.numeric(factor(id_)),
#         group_ = as.numeric(factor(group_)),
#         event_ = as.numeric(factor(event_))
#     )
#
temp <- tibble(
        group_ = rep(letters[1:3], 2),
        event_ = c(rep(0,3), rep(1,3))
    ) %>%
    mutate(id_ = 1:n()) %>%
    add_row(group_ = "c", event_ = 0, id_ = 3) %>%
    add_row(group_ = "a", event_ = 1, id_ = 1) %>%
    relocate(id_,) %>%
    arrange(id_)
# 
# 
# # haha <- class_performance %>% 
# haha <- temp %>%
#     permute_groups_(systematic = TRUE)
#     
#     
# # haha_filtered <- class_performance  %>%
# haha_filtered <- temp %>%
#     permute_groups_(systematic = TRUE) %>%  
#     subset(
#         group_ %in% letters[1:2] &  
#         perm_group_ %in% letters[1:2]
#     )
#     
# haha_specific <- temp %>%
#     subset(group_ %in% letters[1:2])
# 
# haha_purpose <- haha_specific %>%
#     subset(!is.na(group_))  %>%
#     permute_groups_(systematic = TRUE)
#     
# writeLines("\n\n")
# 
# print("haha_specific")
# print(haha_specific)
# 
# print("haha_filtered")
# print(haha_filtered)
# 
# print("haha_purpose")
# print(haha_purpose)
# 
# print(setdiff(haha_filtered, haha_purpose))
# 
# print(setdiff(haha_purpose, haha_filtered))



group_col_name <- "class_teacher"
id_col_name <- "student_id"
event_col_name <- "passed_exam_num"
na_fill <- TRUE


# hehe <- class_performance %>% 
#     select(all_of(c(id_col_name, group_col_name, event_col_name))) %>%
#     mutate(.sep.entry.id = 1:n()) %>%
#     rename(
#         id_ = !!id_col_name,
#         group_ = !!group_col_name,
#         event_ = !!event_col_name
#     ) %>%
#     mutate(
#         group_ = case_when(
#             (na_fill) & is.na(group_) ~ ".NA_group",
#             TRUE ~ group_
#         )
#     )

# haha <- get_p_value(class_performance,
#     group_col_name = group_col_name,
#     id_col_name = id_col_name,
#     event_col_name = event_col_name,
#     systematic = TRUE,
#     na_fill = TRUE)

# haha <- permgp_fn(class_performance,
#     group_col_name = group_col_name,
#     id_col_name = id_col_name,
#     event_col_name = event_col_name,
#     systematic = TRUE,
#     na_fill = TRUE)
# 
# haha <- global_permutation_test(class_performance,
#                     group_col_name = group_col_name,
#                     id_col_name = id_col_name,
#                     event_col_name = event_col_name,
#                     ntrials = 100,
#                     parallel = TRUE,
#                     ranseed = NaN,
#                     systematic = TRUE,
#                     na_fill = FALSE,
#                     verbose = TRUE)

haha <- pairwise_permutation_tests(class_performance,
                                   group_col_name = group_col_name,
                                   id_col_name = id_col_name,
                                   event_col_name = event_col_name,
                                   ntrials = 1000,
                                   reference_group = "placebo",
                                   parallel = TRUE,
                                   global_test_first = TRUE,
                                   verbose = FALSE)
# troubleshoot parallel processing problems here
print(haha)



# I'' back here working on the library that they've asked me to do


























