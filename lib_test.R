rm(list = ls())

setwd("C:/Users/brendon phillips/Documents/GitHub/rothlab_permtest/")

library(devtools)

# detach("package:SepsisTools", unload = TRUE)
# devtools::install_github("brendonphillips/SepsisTools", ref="main", force=TRUE)

devtools::document(); devtools::load_all()

library(SepsisTools)
library(data.table)

data("class_performance")

# perm <- global_permutation_test(
#     haha,
#     group_name = "group",
#     id_name = "part_id",
#     event_name = "event",
#     systematic = TRUE,
#     ntrials = 10000,
#     parallel = TRUE,
#     ranseed = 20000
# )

devtools::document(); devtools::load_all()
