# use the included class_performance data set for the example
data("class_performance")

# carry through the global permutation test
res <- global_permutation_test(class_performance,
                    group_col_name = group_col_name,
                    id_col_name = id_col_name,
                    event_col_name = event_col_name,
                    ntrials = 100,
                    parallel = TRUE,
                    ranseed = NaN,
                    systematic = TRUE,
                    na_fill = FALSE,
                    verbose = TRUE)

# `res` is a list of three statistics
res