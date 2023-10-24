rm(list = ls())

setwd("C:/Users/brendon phillips/Documents/GitHub/rothlab_permtest/")

library(devtools)
library(roxygen2)
library(dplyr)

# detach("package:SepsisTools", unload = TRUE)
# devtools::install_github("brendonphillips/SepsisTools", ref="main", force=TRUE)

# devtools::document(); devtools::load_all()
# 
library(SepsisTools)

roxygen2::update_collate(".")
devtools::document()
devtools::load_all()

data("class_performance")


group_col_name <- "class_teacher"
id_col_name <- "student_id"
event_col_name <- "passed_exam_num"
na_fill <- TRUE


# haha <- permgp_fn(class_performance,
#                   group_col_name = group_col_name,
#                   id_col_name = id_col_name,
#                   event_col_name = event_col_name,
#                   systematic = TRUE,
#                   na_fill = TRUE,
#                   ranseed = 5)
# print(haha)

# haha <- get_p_value(class_performance,
#                     group_col_name = group_col_name,
#                     id_col_name = id_col_name,
#                     event_col_name = event_col_name,
#                     systematic = TRUE,
#                     na_fill = TRUE,
#                     ranseed = 5)
# print(haha)

# hehe <- class_performance %>%
#     standard_table(id_col_name = id_col_name,
#                    group_col_name = group_col_name,
#                    event_col_name = event_col_name,
#                    na_fill = TRUE)
# print(hehe)

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

# temp <- tibble(
#         group_ = rep(letters[1:3], 2),
#         event_ = c(rep(0,3), rep(1,3))
#     ) %>%
#     mutate(id_ = 1:n()) %>%
#     add_row(group_ = "c", event_ = 0, id_ = 3) %>%
#     add_row(group_ = "a", event_ = 1, id_ = 1) %>%
#     relocate(id_,) %>%
#     arrange(id_)

# # haha <- class_performance %>% 
# haha <- temp %>%
#     permute_groups(systematic = TRUE)

# haha <- global_permutation_test(class_performance,
#                     group_col_name = group_col_name,
#                     id_col_name = id_col_name,
#                     event_col_name = event_col_name,
#                     ntrials = 1000,
#                     parallel = TRUE,
#                     ranseed = NaN,
#                     systematic = TRUE,
#                     na_fill = TRUE,
#                     verbose = TRUE)

haha <- pairwise_permutation_tests(class_performance,
                                   group_col_name = group_col_name,
                                   id_col_name = id_col_name,
                                   event_col_name = event_col_name,
                                   ntrials = 100,
                                   reference_group = "Roth",
                                   parallel = FALSE,
                                   global_test_first = TRUE,
                                   verbose = FALSE,
                                   step_down_prodecures = c("BH"),
                                   na_fill = TRUE)



# I'' back here working on the library that they've asked me to do







# lol <- haha %>%
#     map(as_tibble) %>% 
#     reduce(bind_rows) %>% 
#     arrange() %>%
#     cbind(
#         sapply(
#             p.adjust.methods %>% .[. != "none"], 
#             \(x) p.adjust(.$p, x)
#         ) %>% 
#             as.data.frame %>% 
#             tibble %>% 
#             rename_with(~paste0("p_adj_", .x))
#     )
    

# 
# haha <- standard_table(class_performance,
#                        group_col_name = group_col_name,id_col_name = id_col_name,
#                        event_col_name = event_col_name,
#                        na_fill = FALSE)











