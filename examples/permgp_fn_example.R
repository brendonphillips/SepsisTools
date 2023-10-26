data("class_performance")

# calculating the p vakue from the permutation test with the included class_performance data set 
single_permutation(class_performance,
          group_col_name = group_col_name,
          id_col_name = id_col_name,
          event_col_name = event_col_name,
          systematic = TRUE,
          na_fill = TRUE)

# deprecated function; currently an alias for the single_permutation function 
permgp_fn(class_performance,
          group_col_name = group_col_name,
          id_col_name = id_col_name,
          event_col_name = event_col_name,
          systematic = TRUE,
          na_fill = TRUE)