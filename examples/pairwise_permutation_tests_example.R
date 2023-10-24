# load the data set
data(class_performance)

# perform the test  - output a table
output <- pairwise_permutation_tests(class_performance,
                                   group_col_name = group_col_name,
                                   id_col_name = id_col_name,
                                   event_col_name = event_col_name,
                                   ntrials = 1000,
                                   compare_to = "Roth",
                                   parallel = TRUE,
                                   global_test_first = TRUE,
                                   verbose = TRUE,
                                   p_adj_meths = c("BH"),
                                   na_fill = TRUE)

print(output)
# # A tibble: 5 × 8
#  group_name  N_obs N_trials     p   error compare_to p_adj_BH p_adj_holm
#  <chr>       <int>    <int> <dbl>   <dbl> <chr>         <dbl>      <dbl>
# 1 __Global__   3618     1000 0.322 0.0148  NA           NA         NA    
# 2 Grey         1333     1000 0.405 0.0155  Roth          0.54       0.819
# 3 Simpson      1344     1000 0.071 0.00812 Roth          0.284      0.284
# 4 Cumberbatch  1501     1000 0.941 0.00745 Roth          0.941      0.941
# 5 NA           1441     1000 0.273 0.0141  Roth          0.54       0.819