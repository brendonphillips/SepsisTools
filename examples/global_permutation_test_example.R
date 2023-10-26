# use the included class_performance data set for the example
data("class_performance")

# carry through the global permutation test on the class_performance data set
res <- global_permutation_test(class_performance,
                                group_col_name = "class_teacher",
                                id_col_name = "student_id",
                                event_col_name = "passed_quiz_numeric",
                                ntrials = 1000,
                                parallel = TRUE,
                                ranseed = NaN,
                                systematic = TRUE,
                                na_fill = TRUE,
                                verbose = TRUE)


# `res` is a list of four statistics
res
# $p
# [1] 0.337
# 
# $error
# [1] 0.01494761
# 
# $N_trials
# [1] 1000
# 
# $N_obs
# [1] 3618