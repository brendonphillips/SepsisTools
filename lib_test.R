library(devtools)
devtools::install_github("brendonphillips/SepsisTools")

library(SepsisTools)
library(data.table)

haha <- fread("C:/Users/brendon phillips/Documents/GitHub/Roth-Lab-Github-Repository/permutation_test_code/data.csv")

perm <- global_permutation_test(
    haha,
    group_name = "group",
    id_name = "part_id",
    event_name = "event",
)

