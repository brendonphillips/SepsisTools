# load the `class_performance` test data set in the package
data(class_performance)

# encode a number
encode_staffer_IDs(2)

# encode a list of strings
encode_staffer_IDs(list("lisa", "daniel", "diego", "eleanor"))    

# encode a vector of numbers with random seed given for reproducibility
encode_staffer_IDs(1:10, seed=10)

# encode a column of a table, replace the column (out_col_name not given)
encode_staffer_IDs(class_performance, ID_col = "class_teacher", 303902)

# encode a column of a table with given out_col name
encode_staffer_IDs(class_performance, ID_col = "student_id", out_col_name = "encoding", 303902)

# using a magrittr pipe, replace column
encode_staffer_IDs %>% encode_IDs(ID_col = "student_id", seed = 29)

# using a magrittr pipe, without a seed, encodings in aother column
encode_staffer_IDs %>% encode_IDs(ID_col = "student_id", out_col_name = "encoding")
