data("class_performance")

# calculating the p vakue from the permutation test with the included class_performance data set 
get_p_value(class_performance,
          group_name = group_name,
          id_name = id_name,
          event_name = event_name,
          systematic = TRUE,
          na_fill = TRUE)

# deprecated function; currently an alias for the get_p_value function 
permgp_fn(class_performance,
          group_name = group_name,
          id_name = id_name,
          event_name = event_name,
          systematic = TRUE,
          na_fill = TRUE)