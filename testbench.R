rm(list = ls())

setwd("C:/Users/brendon phillips/Documents/GitHub/rothlab_permtest/")

library(devtools)
library(roxygen2)
library(dplyr)
library(readr)

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
event_col_name <- "passed_quiz_numeric"
na_fill <- TRUE

# class_performance <- class_performance %>%
#     encode_staffer_ids(id_col = student_id, ranseed = 25834495) %>%
#     select(-contains("student_id16")) %>%
#     rename_with(~gsub("exam", "quiz", .x)) %>%
#     rename("passed_quiz_numeric" = "passed_quiz_num") %>%
#     group_by(student_id, class_teacher) %>%
#     mutate(
#         quiz_number = sample(1:15, size = n(), replace = FALSE)
#     ) %>%
#     relocate(student_id, class_teacher, quiz_number)
# 
# save(class_performance, file="class_performance.rda")

## generate the staffer_dictionary

# base_table <- fread('base_table.csv')

# exact_match_mangles <- fread("exact_match_mangles.csv")

# staffer_dictionary <- exact_match_mangles %>%
#     encode_staffer_ids(id_col = chrw_name_mangled, out_col_name = "encoding", seed = 455742) %>%
#     select(chrw_ids, encoding) %>%
#     unique %>%
#     separate(chrw_ids, sep = "; ", into = toupper(letters)) %>%
#     select_if(~! all(is.na(.))) %>%
#     pivot_longer(!encoding, names_to = "class", values_to = "chrw_id") %>%
#     subset(!is.na(chrw_id) & chrw_id!="") %>%
#     select(-class) %>%
#     relocate(chrw_id) %>%
#     group_by(chrw_id) %>%
#     slice_min(encoding) %>%
#     ungroup()

# staffer_dictionary %>% 
#     group_by(encoding) %>% 
#     summarise(N = n(), ids = paste(chrw_id, collapse = "; ")) %>% 
#     filter(N > 1)

# save(staffer_dictionary, file = "data/staffer_dictionary.rda")
# fwrite(staffer_dictionary, file = "data/staffer_dictionary.csv")

# haha <- single_permutation(class_performance,
#                   group_col_name = group_col_name,
#                   id_col_name = id_col_name,
#                   event_col_name = event_col_name,
#                   systematic = TRUE,
#                   na_fill = TRUE,
#                   ranseed = 8)
# print(haha)

# haha <- permgp_fn(class_performance,
#                     group_col_name = group_col_name,
#                     id_col_name = id_col_name,
#                     event_col_name = event_col_name,
#                     systematic = TRUE,
#                     na_fill = TRUE,
#                     ranseed = 6)
# print(haha)

# hehe <- class_performance %>%
#     standard_table(id_col_name = id_col_name,
#                    group_col_name = group_col_name,
#                    event_col_name = event_col_name,
#                    na_fill = TRUE)
# print(hehe)

# temp <- class_performance %>%
#     select(student_id, class_teacher, passed_quiz_num) %>%
#     rename(
#         id_ = student_id,
#         group_ = class_teacher,
#         event_ = passed_quiz_num
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

# haha <- tibble(
#     event_ = c(1,0,1,0,1), 
#     group_=c(1,1,2,2,1), 
#     id_=1:5
# ) %>% 
# permute_groups(systematic = TRUE, ranseed = 5)

# haha <- tibble(
#     event_ = c(1,0,1,0,1),
#     group_ = as.character(c(1,1,2,2,1)),
#     id_=1:5
# ) %>%
# single_permutation(systematic = TRUE, ranseed = 6) %>%
#     print()



# haha <- global_permutation_test(class_performance,
#                     group_col_name = "class_teacher",
#                     id_col_name = "student_id",
#                     event_col_name = "passed_quiz_numeric",
#                     ntrials = 100,
#                     parallel = TRUE,
#                     ranseed = 23748923,
#                     systematic = TRUE,
#                     na_fill = TRUE,
#                     verbose = FALSE) %>%
#     print()

haha <- pairwise_permutation_tests(class_performance,
                                   group_col_name = group_col_name,
                                   id_col_name = id_col_name,
                                   event_col_name = event_col_name,
                                   ntrials = 100,
                                   compare_to = "Roth",
                                   parallel = TRUE,
                                   global_test_first = TRUE,
                                   verbose = FALSE,
                                   p_adj_meths = c("BH", "holm"),
                                   na_fill = TRUE,
                                   ranseed = 68) %>%
    print()



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











